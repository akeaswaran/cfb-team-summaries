export interface Percentile {
    season: number
    pctile: number
    gei: number // GEI
    epaPerPlay: number // EPAplay
    successRate: number // success
    yardsPerPlay: number // yardsplay
    epaPerDropback: number // EPAdropback
    epaPerRush: number // EPArush
    yardsPerDropback: number // yardsdropback
    yardsPerRush: number // yardsrush

    explosivePlayRate: number //explosive
    thirdDownSuccessRate: number // third_down_success
    redZoneSuccessRate: number // red_zone_success

    passingSuccessRate: number
    passingExplosivePlayRate: number //explosive
    rushingSuccessRate: number
    rushingExplosivePlayRate: number //explosive

    playStuffedRate: number // play_stuffed
    havocRate: number //havoc

    lineYards: number
    rushOpportunityRate: number

    nonExplosiveEpaPerPlay: number
    earlyDownEpaPerPlay: number
    earlyDownSuccessRate: number
    lateDownSuccessRate: number
    thirdDownDistance: number
}