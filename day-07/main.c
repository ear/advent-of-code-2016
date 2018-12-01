#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAXIPS 2048
#define MAXIPv7LEN 1024

struct state
{
	char ips[MAXIPS][MAXIPv7LEN];
	size_t ip_count;
	size_t tls_count;
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

int
main (void)
{
	struct state s = { 0 };
	printf("total:\t%ld\n", parse(&s));
	printf("tls:\t%ld\n", countTLS(&s));
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

