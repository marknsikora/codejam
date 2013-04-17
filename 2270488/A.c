#include <stdlib.h>
#include <stdio.h>

#include <glib.h>

int
main(int argc, char **argv)
{
    int cur, i, j, tests;
    char *board[4];
    char sideways_board[4][5];
    char left_diagonal[5], right_diagonal[5];
    size_t n[4];
    char player;

    GRegex *x_win, *o_win, *draw;

    x_win = g_regex_new("^[XT]{4,4}$", G_REGEX_OPTIMIZE, 0, NULL);
    o_win = g_regex_new("^[OT]{4,4}$", G_REGEX_OPTIMIZE, 0, NULL);
    draw = g_regex_new("^[XOT]{4,4}$", G_REGEX_OPTIMIZE, 0, NULL);

    for (i = 0; i < 4; i++) {
        board[i] = NULL;
    }

    scanf("%d\n", &tests);

    for (cur = 1; cur <= tests; cur++) {
        printf("Case #%d: ", cur);

        for (i = 0; i < 4; i++) {
            getline(&board[i], &n[i], stdin);
        }

        for (i = 0; i < 4; i++) {
            for (j = 0; j < 4; j++) {
                sideways_board[i][j] = board[j][i];
            }
            sideways_board[i][4] = '\0';
            board[i][4] = '\0';

            left_diagonal[i] = board[i][i];
            right_diagonal[i] = board[i][3-i];
        }
        left_diagonal[4] = '\0';
        right_diagonal[4] = '\0';

        player = '\0';
        for (i = 0; i < 4; i++) {
            if (g_regex_match(x_win, board[i], 0, NULL) || 
                g_regex_match(x_win, sideways_board[i], 0, NULL)) {
                player = 'X';
                break;
            }

            if (g_regex_match(o_win, board[i], 0, NULL) || 
                g_regex_match(o_win, sideways_board[i], 0, NULL)) {
                player = 'O';
                break;
            }
        }

        if (!player) {
            if (g_regex_match(o_win, left_diagonal, 0, NULL) ||
                g_regex_match(o_win, right_diagonal, 0, NULL)) {
                player = 'O';
            } else if (g_regex_match(x_win, left_diagonal, 0, NULL) ||
                       g_regex_match(x_win, right_diagonal, 0, NULL)) {
                player = 'X';
            }
        }

        if (player) {
            printf("%c won\n", player);
        } else {
            for (i = 0; i < 4; i++) {
                if (!g_regex_match(draw, board[i], 0, NULL)) {
                    printf("Game has not completed\n");
                    break;
                }
            }

            if (i == 4) {
                printf("Draw\n");
            }
        }

        scanf("\n");
    }

    return EXIT_SUCCESS;
}
