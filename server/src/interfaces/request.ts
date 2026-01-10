import { SummaryType } from "../enums/summary-types"
import { TeamSummary } from "./overall-summary"
import { PlayerSummary } from "./player-summary"

export interface SummaryRequest {
    year?: number
    team?: string
    type?: SummaryType
}

export type Summary = PlayerSummary | TeamSummary; 