import { SummaryType } from "../enums/summary-types"
import { TeamSummary } from "./overall-summary"
import { PlayerSummary } from "./player-summary"

export interface SummaryRequest {
    year?: number
    team?: string
    type?: SummaryType
}

export interface PercentileRequest {
    year?: number
    pctile?: number // has to be 0.01 to 0.99
}


export type Summary = PlayerSummary | TeamSummary; 