
#include "poker_defs.h"
#include "enumdefs.h"
#include "inlines/eval.h"
#include "pokereval_export.h"
#include <math.h>


#define CARDS_WITH_RANK(mask, rank) \
    (StdDeck_CardMask_CARD_IS_SET(mask,StdDeck_MAKE_CARD(rank, 0)) + \
    StdDeck_CardMask_CARD_IS_SET(mask,StdDeck_MAKE_CARD(rank, 1)) + \
    StdDeck_CardMask_CARD_IS_SET(mask,StdDeck_MAKE_CARD(rank, 2)) + \
    StdDeck_CardMask_CARD_IS_SET(mask,StdDeck_MAKE_CARD(rank, 3)))

double est_highcard_n(int rank, StdDeck_CardMask cards, int picks)
{
    switch(CARDS_WITH_RANK(cards, rank)) {
    case 0:
        return 1.0-pow(48.0/52.0,(double)picks);
    default:
        return 1.0;
    }
}
double est_onepair_n(int rank, StdDeck_CardMask cards, int picks)
{
    double chanceOfPickingRank = 4.0/52.0;
    double chanceOfNotRepickingRank = 48.0/51.0;
    switch(CARDS_WITH_RANK(cards, rank)) {
    case 0:
        if(picks < 2)
            return 0;
        else
            return chanceOfPickingRank * (1.0 - pow(chanceOfNotRepickingRank,(double)picks-1));
    case 1:
        return 1.0-pow(chanceOfNotRepickingRank,(double)picks);
    default:
        return 1.0;
    }
}
double est_twopair_n(int rank, StdDeck_CardMask cards, int picks)
{
    double pickOne = 4.0/52.0;
    double pickTwo = 3.0/51.0;
    double other=0.0;
    int i;
    switch(CARDS_WITH_RANK(cards, rank)) {
    case 0:
        for(i=0;i<rank;i++)
            other += est_onepair_n(i,cards,picks-2);
        return pickOne * pickTwo * other;
    case 1:
        for(i=0;i<rank;i++)
            other += est_onepair_n(i,cards,picks-1);
        return pickTwo * other;
    default:
        for(i=0;i<rank;i++)
            other += est_onepair_n(i,cards,picks);
        return other;
    }
}				
double est_threeofakind_n(int rank, StdDeck_CardMask cards, int picks)
{
    double pickOne = 4.0/52.0;
    double pickTwo = 3.0/51.0;
    double chanceOfNotRepickingRank = 48.0/50.0;
    switch(CARDS_WITH_RANK(cards, rank)) {
    case 0:
        if(picks < 3)
            return 0;
        else
            return pickOne * pickTwo * (1.0 - pow(chanceOfNotRepickingRank,(double)picks-2));
    case 1:
        if(picks < 2)
            return 0;
        else
            return pickTwo * (1.0 - pow(chanceOfNotRepickingRank,(double)picks-1));
    case 2:
        return 1.0-pow(chanceOfNotRepickingRank,(double)picks);
    default:
        return 1.0;
    }
}				
