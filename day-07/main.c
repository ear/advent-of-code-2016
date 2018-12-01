#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAXABA 1024

struct ssl
{
	struct
	{
		char a;
		char b;
	} ab[MAXABA];
	size_t count;
};

void
dump (struct ssl * s)
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
collectABA (struct ssl *, char *);

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
	struct ssl s_out = { 0 };
	struct ssl s_in  = { 0 };

	char * start = str;
	while (*start)
	{
		char * end = start;
		while (*end != '\0' && *end != '[' && *end != ']')
			++end;
		if (*end == ']')
		{
			*end = '\0';
			collectABA(&s_in, start);
		}
		else
		{
			*end = '\0';
			collectABA(&s_out, start);
		}
		start = ++end;
	}
	//printf("%.20s...\t%zu\t%zu\n", str, s_out.count, s_in.count);
	//dump(&s_out);
	//dump(&s_in);

	if (s_out.count && s_in.count)
		for (size_t i = 0; i < s_out.count; ++i)
			for (size_t j = 0; j < s_in.count; ++j)
				if (  (s_out.ab[i].a == s_in.ab[j].b)
				   && (s_out.ab[i].b == s_in.ab[j].a) )
					return true;

	return false;
}

size_t
collectABA (struct ssl * s, char * str)
{
	char * p = str;

	for (size_t i = 0; *p != '\0'; ++i, ++p)
	{
		if (i > 1)
		{
			if ( *(p) == *(p-2) && *(p) != *(p-1) )
			{
				s->ab[s->count].a = *p;
				s->ab[s->count].b = *(p-1);
				++s->count;
			}
		}
	}

//	printf("%.10s...\t%zu\n", str, s->count);

	return s->count;
}

