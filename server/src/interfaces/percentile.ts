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

    explosivePlayRate: number //explosive
    thirdDownSuccessRate: number // third_down_success
    redZoneSuccessRate: number // red_zone_success

    playStuffedRate: number // play_stuffed
    havocRate: number //havoc
}