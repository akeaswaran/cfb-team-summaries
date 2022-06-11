export interface Summary {
    teamId: number
    team: string
    season: number
    offensive: Segment & PlaysSegment
    defensive: Segment & PlaysSegment
    differential: Segment
}

export interface PlaysSegment {
    totalPlays: number
    playsPerGame: number
    playsPerGameRank: number
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
