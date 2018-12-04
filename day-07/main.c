#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAXABA 1024

struct ab
{
	char a;
	char b;
};

struct aba
{
	struct ab ab[MAXABA];
	size_t count;
};

void
dump (struct aba * s)
{
	for (size_t i = 0; i < s->count; ++i)
		printf("\t%c%c%c", s->ab[i].a, s->ab[i].b, s->ab[i].a);
	printf("\n");
}

#define MAXIPS 2048
#define MAXIPv7LEN 1024

struct state
{
	char ips[MAXIPS][MAXIPv7LEN];
	size_t ip_count;
	size_t tls_count;
	size_t ssl_count;
};

size_t
parse (struct state *);

size_t
countTLS (struct state *);

bool
isTLS (char *);

bool
isTLS_ (char *, bool);

bool
hasABBA (char *);

size_t
countSSL (struct state *);

bool
isSSL (char *);

size_t
collectABA (struct aba *, char *, char *);

int
main (void)
{
	struct state s = { 0 }, s1;
	printf("total:\t%ld\n", parse(&s));
	memcpy((void *)&s1, (void *)&s, sizeof(struct state));
	printf("tls:\t%ld\n", countTLS(&s));
	printf("ssl:\t%ld\n", countSSL(&s1));
}

size_t
parse (struct state * s)
{
	char (*ip)[MAXIPv7LEN] = s->ips;
	while (scanf("%s", *ip++) > 0)
		++s->ip_count;
	return s->ip_count;
}

size_t
countTLS (struct state * s)
{
	for (size_t i = 0; i < s->ip_count; ++i)
	{
		if (isTLS(s->ips[i]))
			++s->tls_count;
	}
	return s->tls_count;
}

bool
isTLS (char * ip)
{
	return isTLS_(ip, false);
}

bool
isTLS_ (char * ip, bool hasABBAoutside)
{
	char * p = ip;
	if (*p == '\0')
		return hasABBAoutside;
	while (*p != '\0' && *p != '[' && *p != ']')
		++p;
	bool isInside = *p == ']';
	*p = '\0';
	if (isInside && hasABBA(ip))
		return false;
	return isTLS_(++p, hasABBAoutside || hasABBA(ip));
}

bool
hasABBA (char * str)
{
	char * p = str;
	for (size_t i = 0; *p != '\0'; ++i, ++p)
	{
		if (i >= 3)
		{
			if (  (*p != *(p-1))
			   && (*p == *(p-3))
			   && (*(p-2) == *(p-1)))
			{
				return true;
			}
		}
	}
	return false;
}

size_t
countSSL (struct state * s)
{
	for (size_t i = 0; i < s->ip_count; ++i)
	{
		if (isSSL(s->ips[i]))
			++s->ssl_count;
	}
	return s->ssl_count;
}

bool
isSSL (char * str)
{
	struct aba out = { 0 }, in = { 0 };

	char * start = str;
	char * end   = start;
	while (*start)
	{
		while (*end != '\0' && *end != '[' && *end != ']')
			++end;
		collectABA( (*end == ']') ? &in : &out, start, end );
		start = ++end;
	}
	//printf("%.20s...\t%zu\t%zu\n", str, out.count, in.count);
	//dump(&out);
	//dump(&in);

	if (out.count && in.count)
		for (size_t i = 0; i < out.count; ++i)
			for (size_t j = 0; j < in.count; ++j)
				if (  (out.ab[i].a == in.ab[j].b)
				   && (out.ab[i].b == in.ab[j].a) )
					return true;

	return false;
}

size_t
collectABA (struct aba * s, char * begin, char * end)
{
	struct ab * ab = &(s->ab[s->count]);
	char * p = begin;

	while (p < end - 2)
	{
		if ( *p == *(p+2) && *p != *(p+1) )
		{
			ab->a = *p;
			ab->b = *(p+1);
			++ab;
			++s->count;
		}
		++p;
	}

//	printf("%.10s...\t%zu\n", str, s->count);

	return s->count;
}

