#include "poker_defs.h"
#include "enumdefs.h"
#include "inlines/eval.h"

#define hs_StdDeck_CardMask uint64
#define hs_toCardMask(m)    ((StdDeck_CardMask){.cards_n = (m)})
#define hs_fromCardMask(m)  ((m).cards_n)

hs_StdDeck_CardMask hs_StdDeck_MASK(int cardIndex)
{
    return hs_fromCardMask(StdDeck_MASK(cardIndex));
}

int hs_StdDeck_maskToString(hs_StdDeck_CardMask m, char *s)
{
    return StdDeck_maskToString(hs_toCardMask(m), s);
}

int hs_StdDeck_numCards(hs_StdDeck_CardMask m)
{
  return StdDeck_numCards(hs_toCardMask(m));
}
int hs_StdDeck_MAKE_CARD(int rank,int suit)
{
    return StdDeck_MAKE_CARD(rank,suit);
}
HandVal hs_StdDeck_StdRules_EVAL_N(hs_StdDeck_CardMask m, int n_cards)
{
    return StdDeck_StdRules_EVAL_N(hs_toCardMask(m), n_cards);
}

typedef void (callback)(hs_StdDeck_CardMask);

void hs_DECK_ENUMERATE_5_CARDS_D(callback cb, hs_StdDeck_CardMask hs_dead)
{
    StdDeck_CardMask var, dead = hs_toCardMask(hs_dead);
    DECK_ENUMERATE_5_CARDS_D(StdDeck, var, dead, cb(hs_fromCardMask(var)););
}

int hs_enumExhaustive(enum_game_t game, hs_StdDeck_CardMask pockets[],
               hs_StdDeck_CardMask board, hs_StdDeck_CardMask dead,
               int npockets, int nboard, int orderflag,
               enum_result_t *result)
{
    return enumExhaustive(game, (StdDeck_CardMask*) pockets,
               hs_toCardMask(board), hs_toCardMask(dead),
               npockets, nboard, orderflag, result);
}
