export interface PlayerSummary {
    teamId: number
    team: string
    season: number
    name: string
    playerId: string

    advanced: AdvancedPlayerStats
    statistics: PlayerStatistics
}

export type PlayerStatistics = (PassingStats | RushingStats | ReceivingStats) & BasePlayerStats;

interface BasePlayerStats {
    plays: number
    games: number
    playsPerGame: number
    yards: number
    yardsPerPlay: number
    yardsPerGame: number

    yardsRank: number
    yardsPerPlayRank: number
    yardsPerGameRank: number
}

export interface AdvancedPlayerStats {
    totalEPA: number
    epaPerPlay: number
    epaPerGame: number
    successRate: number

    totalEPARank: number
    epaPerPlayRank: number
    epaPerGameRank: number
    successRateRank: number
}

export interface PassingStats {
    completions: number
    attempts: number
    dropbacks: number
    completionPct: number
    touchdowns: number
    sacks: number
    sackYards: number
    sackAdjustedYards: number
    yardsPerDropback: number

    interceptions: number
    detmer: number
    detmerPerGame: number

    completionPctRank: number
    detmerRank: number
    detmerPerGameRank: number

    sackAdjustedYardsRank: number
    yardsPerDropbackRank: number
}

export interface RushingStats {
    touchdowns: number
    fumbles: number
}

export interface ReceivingStats {
    catches: number
    targets: number
    catchPct: number
    touchdowns: number
    fumbles: number

    catchPctRank: number
}