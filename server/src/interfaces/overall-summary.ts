export interface TeamSummary {
    teamId: number
    team: string
    season: number
    offensive: SituationWrapper<Segment & PlaysSegment & SituationalSuccess>
    defensive: SituationWrapper<Segment & PlaysSegment & SituationalSuccess>
    differential: SituationWrapper<Segment>
}

export interface PlaysSegment {
    totalPlays: number
    playsPerGame: number
    playsPerGameRank: number
}

export interface DrivesSegment {
    totalAvailableYards: number
    totalGainedYards: number
    availableYardsPct: number

    totalAvailableYardsRank: number
    totalGainedYardsRank: number
    availableYardsPctRank: number
}

export interface SituationWrapper<T> {
    overall: T & DrivesSegment
    passing: T
    rushing: T
}

export interface Segment {
    totalEPA: number
    epaPerPlay: number
    epaPerDrive: number
    epaPerGame: number
    successRate: number
    startingFP: number
    yards: number
    yardsPerPlay: number
    yardsPerGame: number

    totalEPARank: number
    epaPerPlayRank: number
    epaPerDriveRank: number
    epaPerGameRank: number
    successRateRank: number
    startingFPRank: number
    yardsRank: number
    yardsPerPlayRank: number
    yardsPerGameRank: number
}

export interface SituationalSuccess {
    stuffedPlayRate: number
    redZoneSuccessRate: number
    thirdDownSuccessRate: number
    thirdDownDistance: number
    lateDownSuccessRate: number
    earlyDownEPAPerPlay: number

    stuffedPlayRateRank: number
    redZoneSuccessRateRank: number
    thirdDownSuccessRateRank: number
    thirdDownDistanceRank: number
    lateDownSuccessRateRank: number
    earlyDownEPAPerPlayRank: number

    havocRate: number
    havocRateRank: number

    explosiveRate: number
    explosiveRateRank: number

    nonExplosiveEpaPerPlay: number
    nonExplosiveEpaPerPlayRank: number
    lineYards: number
    lineYardsRank: number
    opportunityRate: number
    opportunityRateRank: number
}
