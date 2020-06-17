#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void
die (const char * fmt, ...)
{
  va_list ap;
  fputs ("spliticnf: error: ", stderr);
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  fputc ('\n', stderr);
  exit (1);
}

static void
msg (const char * fmt, ...)
{
  va_list ap;
  fputs ("[spliticnf] ", stdout);
  va_start (ap, fmt);
  vprintf (fmt, ap);
  va_end (ap);
  fputc ('\n', stdout);
  fflush (stdout);
}

#define MAX (1u<<29)

static int * lits;
static unsigned num_lits;
static unsigned size_lits;

static int ** clauses;
static unsigned num_clauses;
static unsigned size_clauses;

static int ** cubes;
static unsigned num_cubes;
static unsigned size_cubes;

#define PUSH(S,E) \
do { \
  if (size_ ## S == num_ ## S) \
    { \
      if (size_ ## S == (1u<<31)) \
	die ("stack overflow in '" #S "'"); \
      size_ ## S = size_ ## S ? 2 * size_ ## S : 1; \
      S = realloc (S, size_ ## S * sizeof *S); \
      if (!S) \
	die ("out of memory enlarging '" #S "'"); \
    } \
  S[num_ ## S++] = (E); \
} while (0)

static size_t lineno = 1;
static const char * input_path;
static FILE * input_file;

static void
error (const char * fmt, ...)
{
  va_list ap;
  fprintf (stderr,
    "spliticnf: parse error: line %zu in '%s': ",
    lineno, input_path);
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  fputc ('\n', stderr);
  exit (1);
}

static int
nextch (void)
{
  int res = getc (input_file);
  if (res == '\n')
    lineno++;
  return res;
}

static int *
copy (void)
{
  size_t bytes = num_lits * sizeof (int);
  int * res = malloc (bytes);
  if (!res)
    die ("failed to allocate literals");
  memcpy (res, lits, num_lits * sizeof (int));
  num_lits = 0;
  return res;
}

static void
unexpected (int ch)
{
  if (ch == EOF)
    error ("unexpected end-of-file");
  else if (isprint (ch))
    error ("unexpected character '%c'", (char) ch);
  else
    error ("unexpected character code 0x%02d", (int) ch);
}

static void
parse (void)
{
  input_file = fopen (input_path, "r");
  if (!input_file)
    die ("can not open '%s' for reading", input_path);
  int ch;
  do
    {
      ch = nextch ();
      if (ch == EOF)
	error ("unexpected end-of-file before header");
      if (ch == 'c')
	{
	  while ((ch = nextch ()) != '\n')
	    if (ch == EOF)
	      error ("unexpected end-of-file in header comment");
	  continue;
	}
      if (ch != 'p')
	unexpected (ch);
    }
  while (ch != 'p');

  if (nextch () != ' ' ||
      nextch () != 'i' ||
      nextch () != 'n' ||
      nextch () != 'c' ||
      nextch () != 'c' ||
      nextch () != 'n' ||
      nextch () != 'f' ||
      nextch () != '\n')
    error ("invalid header line (expected 'p inccnf')");

  int lit = 0;
  bool cnf = true;
  bool cube = false;
  for (;;)
    {
      ch = nextch ();
      if (ch == ' ' || ch == '\n' || ch == '\t')
	continue;
      if (ch == 'c')
	{
	  while ((ch = nextch ()) != '\n')
	    if (ch == EOF)
	      error ("unexpected end-of-file in comment");
	  continue;
	}
      if (ch == EOF)
	break;
      if (ch == 'a')
	{
	  if (cube)
	    error ("two 'a' in a row");
	  if (lit)
	    error ("trailing zero missing");
	  cube = true;
	  continue;
	}
      int sign = 1;
      if (ch == '-')
	{
	  ch = nextch ();
	  if (!isdigit (ch))
	    error ("expected digit after '-'");
	  if (ch == '0')
	    error ("expected non-zero digit after '-'");
	  sign = -1;
	}
      else if (!isdigit (ch))
	unexpected (ch);
      int idx = ch - '0';
      while (isdigit (ch = nextch ()))
	{
	  if (INT_MAX/10 < idx)
	    error ("variable index too large");
	  idx *= 10;
	  int digit = ch - '0';
	  if (INT_MAX - digit < idx)
	    error ("variable index too large");
	  idx += digit;
	}
      if (ch == 'c')
	{
	  while ((ch = nextch ()) != '\n')
	    if (ch == EOF)
	      error ("unexpected end-of-file in comment");
	}
      lit = sign * idx;
      if (ch != ' ' && ch != '\n' && ch != '\t')
	unexpected (ch);
      PUSH (lits, lit);
      if (lit)
	continue;
      if (!cnf && !cube)
	error ("cubes have to follow clauses");
      if (cnf)
	PUSH (clauses, copy ());
      else
	PUSH (cubes, copy ());
      if (cube)
	cnf = false;
      cube = false;
    }
  if (lit)
    error ("unexpected end-of-file without zero");
  if (cube)
    error ("unexpected end-of-file after 'a'");
  fclose (input_file);
  msg ("parsed %u clauses and %u cubes", num_clauses, num_cubes);
}

static bool
is_positive_number (const char * str)
{
  if (!*str)
    return false;
  for (const char * p = str; *p; p++)
    if (!isdigit (*p))
      return false;
  return true;
}

static unsigned partitions;
static unsigned size;
static unsigned rest;

static void
parse_patitions_argument (const char * arg)
{
  const char * p = arg;
  while (*p)
    {
      if (UINT_MAX/10 < partitions)
	die ("partitions argument '%s' too large", arg);
      partitions *= 10;
      assert (isdigit (*p));
      unsigned digit = *p - '0';
      if (UINT_MAX - digit < partitions)
	die ("partitions argument '%s' too large", arg);
      partitions += digit;
      p++;
    }
}

static char * base;
static char * suffix;
static unsigned digits;
static char * partition_path;
static char partition_format[32];
static bool strided = true;

static void
determine_base (void)
{
  base = malloc (strlen (input_path) + 32);
  if (!base)
    die ("failed to copy path");
  strcpy (base, input_path);
  char * end = base + strlen (base), * p;
  for (p = end; p > base && *p != '.'; p--)
    ;
  if (!strcmp (p, ".icnf") ||
      !strcmp (p, ".inccnf") ||
      !strcmp (p, ".incnf") ||
      !strcmp (p, ".cnf") ||
      !strcmp (p, ".dimacs"))
    {
      suffix = strdup (p);
      end = p;
      *p = 0;
    }
  else
    suffix = strdup ("");
  sprintf (end, "%s%upart", strided ? "strided" : "split", partitions);
  unsigned long expdigits = 10;
  digits = 1;
  while (partitions >= expdigits)
    {
      digits++;
      expdigits *= 10;
    }
  if (*suffix)
    msg ("base path '%s' digits %u suffix '%s'", base, digits, suffix);
  else
    msg ("base path '%s' digits %u (no suffix)", base, digits);
  partition_path = malloc (strlen (input_path) + 32);
  if (!partition_path)
    die ("failed to allocate partition path");
  sprintf (partition_format, "%%s%%0%uu%%s", digits);
  msg ("partition path format '%s'", partition_format);
}

static unsigned written;

static void
write_partition (unsigned i)
{
  assert (i < partitions);
  sprintf (partition_path, partition_format, base, i + 1, suffix);
  unsigned begin, end, count, inc;
  if (strided)
    {
      begin = i;
      end = num_cubes;
      inc = partitions;
      count = size;
      if (i < rest)
	count++;
    }
  else
    {
      begin = written;
      end = begin + size;
      inc = 1;
      if (i < rest)
	end++;
      count = end - begin;
    }
  if (count == 1)
    msg ("writing single cube [%u] to '%s'", begin, partition_path);
  else if (strided)
    msg ("writing %u cubes [%u,%u,..] to '%s'",
         count, begin, begin + inc, partition_path);
  else
    msg ("writing %u cubes [%u..%u] to '%s'",
         count, begin, end - 1, partition_path);
  FILE * partition_file = fopen (partition_path, "w");
  if (!partition_file)
    error ("can not open and write to '%s'", partition_path);
  fprintf (partition_file, "p inccnf\n");
  for (unsigned i = 0; i < num_clauses; i++)
    {
      const int * clause = clauses[i];
      for (const int * p = clause; *p; p++)
	fprintf (partition_file, "%d ", *p);
      fputs ("0\n", partition_file);
    }
  for (unsigned i = begin; i < end; i += inc)
    {
      fputs ("a ", partition_file);
      const int * cube = cubes[i];
      for (const int * p = cube; *p; p++)
	fprintf (partition_file, "%d ", *p);
      fputs ("0\n", partition_file);
    }
  fclose (partition_file);
  written = end;
}

static void
usage (void)
{
  printf ("usage: spliticnf [-h|-b|--blocked|-s|--strided] <input> <partitions>\n");
  exit (0);
}

int
main (int argc, char ** argv)
{
  for (int i = 1; i < argc; i++)
  {
    if (!strcmp (argv[i], "-h"))
      usage ();
    else if (!strcmp (argv[i], "-b") || !strcmp (argv[i], "--blocked"))
      strided = false;
    else if (!strcmp (argv[i], "-s") || !strcmp (argv[i], "--strided"))
      strided = true;
    else if (argv[i][0] == '-')
      die ("invalid argument '%s' (try '-h')", argv[i]);
    else if (is_positive_number (argv[i]))
      {
	if (argv[i][0] == '0')
	  die ("invalid partitions argument '%s'", argv[i]);
	if (partitions)
	  die ("two partitions arguments '%u' and '%s' (try '-h')",
	       partitions, argv[i]);
	parse_patitions_argument (argv[i]);
	assert (partitions);
      }
    else if (input_path)
      die ("two file arguments '%s' and '%s' (try '-h')",
           input_path, argv[i]);
    else
      input_path = argv[i];
  }
  if (!input_path)
    die ("file argument missing (try '-h')");
  if (!partitions)
    die ("split argument missing (try '-h')");
  msg ("parsing '%s'", input_path);
  parse ();
  if (partitions > num_cubes)
    die ("partitions argument '%u' exceeds number of cubes '%u'",
         partitions, num_cubes);
  size = num_cubes / partitions;
  rest = num_cubes % partitions;
  msg ("split size %u with rest %u", size, rest);
  determine_base ();
  for (unsigned partition = 0; partition < partitions; partition++)
    write_partition (partition);
  msg ("wrote all %u cubes", written);
  assert (written == num_cubes);
  for (unsigned i = 0; i < num_clauses; i++)
    free (clauses[i]);
  for (unsigned i = 0; i < num_cubes; i++)
    free (cubes[i]);
  free (base);
  free (suffix);
  free (partition_path);
  free (clauses);
  free (cubes);
  free (lits);
  return 0;
}
