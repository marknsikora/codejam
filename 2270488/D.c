#include <stdlib.h>
#include <stdio.h>

typedef struct _Chest Chest;
struct _Chest {
    int required;
    int n;
    int *keys;
};

static int tresure(Chest **chests, int nchests, int *keys, int nkeys, int *order);


int
main(int argc, char **argv)
{
    int cur, i, j, k, n, tests;
    int *keys, *order;
    Chest **chests;
    
    chests = (Chest**) malloc(200*sizeof(Chest*));
    for (i = 0; i < 200; i++) {
        chests[i] = (Chest*) malloc(sizeof(Chest));
        chests[i]->keys = NULL;
    }

    order = (int*) malloc(200*sizeof(int));

    keys = (int*) malloc(400*sizeof(int));

    scanf("%d\n", &tests);

    for (cur = 1; cur <= tests; cur++) {
        printf("Case #%d: ", cur);

        scanf("%d %d\n", &k, &n);

        for (i = 0; i < k; i++) {
            scanf("%d", &keys[i]);
        }

        for (i = 0; i < n; i++) {
            scanf("%d %d", &chests[i]->required, &chests[i]->n);

            chests[i]->keys = (int*) malloc(chests[i]->n*sizeof(int));
            for (j = 0; j < chests[i]->n; j++) {
                scanf("%d", &chests[i]->keys[j]);
            }
        }

        if (tresure(chests, n, keys, k, order)) {
            for (i = 0; i < n; i++) {
                printf("%d ", order[i]+1);
            }
            printf("\n");
        } else {
            printf("IMPOSSIBLE\n");
        }

        for (i = 0; i < n; i++) {
            free(chests[i]->keys);
            chests[i]->keys = NULL;
        }
    }

    return EXIT_SUCCESS;
}

static int
tresure(Chest **chests, int nchests, int *keys, int nkeys, int *order)
{
    int i, j, k, count, result;
    Chest *chest;
    int key;

    count = 0;
    for (i = 0; i < nchests; i++) {
        if (chests[i]) {
            count++;
        }
    }

    if (count == 0) {
        return 1;
    }

    for (i = 0; i < nchests; i++) {
        for (j = 0; j < nkeys; j++) {
            if (chests[i] && chests[i]->required == keys[j]) {
                
                chest = chests[i];
                chests[i] = NULL;

                key = keys[j];
                keys[j] = 0;

                for (k = 0; k < chest->n; k++) {
                    keys[nkeys+k] = chest->keys[k];
                }

                result = tresure(chests, nchests, keys, nkeys+chest->n, order);
                chests[i] = chest;
                keys[j] = key;

                if (result) {
                    order[nchests-count] = i;
                    return 1;
                }
            }
        }
    }

    return 0;
}
