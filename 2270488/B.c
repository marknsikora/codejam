#include <stdlib.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
    int cur, i, j, n, m, success, tests;
    int **lawn, *vert_max, *horz_max;

    lawn = (int**) malloc(100*sizeof(int*));
    for (i = 0; i < 100; i++) {
        lawn[i] = (int*) malloc(100*sizeof(int));
    }

    vert_max = (int*) malloc(100*sizeof(int));
    horz_max = (int*) malloc(100*sizeof(int));

    scanf("%d\n", &tests);

    for (cur = 1; cur <= tests; cur++) {
        printf("Case #%d: ", cur);

        scanf("%d %d\n", &n, &m);

        for (i = 0; i < n; i++) {
            for (j = 0; j < m; j++) {
                scanf("%d", &lawn[i][j]);
            }
        }

        for (i = 0; i < n; i++) {
            horz_max[i] = lawn[i][0];

            for (j = 1; j < m; j++) {
                if (lawn[i][j] > horz_max[i]) {
                    horz_max[i] = lawn[i][j];
                }
            }
        }

        for (i = 0; i < m; i++) {
            vert_max[i] = lawn[0][i];

            for (j = 1; j < n; j++) {
                if (lawn[j][i] > vert_max[i]) {
                    vert_max[i] = lawn[j][i];
                }
            }
        }

        success = 1;
        for (i = 0; success && i < n; i++) {
            for (j = 0; j < m; j++) {
                if (lawn[i][j] < vert_max[j] &&
                    lawn[i][j] < horz_max[i]) {
                    success = 0;
                    break;
                }
            }
        }

        printf("%s\n", success ? "YES" : "NO");
    }

    return EXIT_SUCCESS;
}
