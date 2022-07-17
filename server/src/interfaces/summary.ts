export interface Summary {
    teamId: number
    team: string
    season: number
    offensive: SituationWrapper<Segment & PlaysSegment>
    defensive: SituationWrapper<Segment & PlaysSegment>
    differential: SituationWrapper<Segment>
}

export interface PlaysSegment {
    totalPlays: number
    playsPerGame: number
    playsPerGameRank: number
}

export interface SituationWrapper<T> {
    overall: T
    passing: T
    rushing: T
}

export interface Segment {
    totalEPA: number
    epaPerPlay: number
    epaPerGame: number
    successRate: number
    startingFP: number

    totalEPARank: number
    epaPerPlayRank: number
    epaPerGameRank: number
    successRateRank: number
    startingFPRank: number
}
