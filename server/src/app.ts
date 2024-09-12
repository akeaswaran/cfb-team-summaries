
// Import the express in typescript file
import express from 'express';
import process from 'process';
import csvtojson from 'csvtojson';
import { TeamSummary } from './interfaces/overall-summary';
import { SummaryType } from './enums/summary-types';
import { Summary, SummaryRequest } from './interfaces/request';
import { PassingStats, PlayerStatistics, PlayerSummary, ReceivingStats, RushingStats, AdvancedPlayerStats } from './interfaces/player-summary';
import bodyParser from 'body-parser';
import morgan from 'morgan';
import { Percentile } from './interfaces/percentile';
import fs from 'fs';

const lastUpdated = fs.statSync("./data/2024/overall.csv")['mtime'];

function parseName(item: any, type: SummaryType): string {
    let key = 'name';
    if (type === SummaryType.Passing) {
        key = 'passer_player_name';
    } else if (type === SummaryType.Rushing) {
        key = 'rusher_player_name';
    } else if (type === SummaryType.Receiving) {
        key = 'receiver_player_name';
    }
    return item[key];
}

function parseAdvanced(item: any): AdvancedPlayerStats {
    let stats: any = {
        totalEPA: item.TEPA,
        epaPerPlay: item.EPAplay,
        epaPerGame: item.EPAgame,
        successRate: item.success,
    
        totalEPARank: item.TEPA_rank,
        epaPerPlayRank: item.EPAplay_rank,
        epaPerGameRank: item.EPAgame_rank,
        successRateRank: item.success_rank
    }
    Object.keys(stats).forEach(key => {
        stats[key] = (stats[key] == "NA") ? null : parseFloat(stats[key])
    })
    return <AdvancedPlayerStats>stats;
}

function parseStats(item: any, type: SummaryType): PlayerStatistics {
    let stats: any = {
        plays: item.plays,
        games: item.games,
        playsPerGame: item.playsgame,
        yards: item.yards,
        yardsPerPlay: item.yardsplay,
        yardsPerGame: item.yardsgame,
    
        yardsRank: item.yards_rank,
        yardsPerPlayRank: item.yardsplay_rank,
        yardsPerGameRank: item.yardsgame_rank
    }
    if (type === SummaryType.Passing) {
        (<PassingStats>stats).completions = item.comp;
        (<PassingStats>stats).attempts = item.att;
        (<PassingStats>stats).completionPct = item.comppct;
        (<PassingStats>stats).touchdowns = item.passing_td;
        (<PassingStats>stats).sacks = item.sacked;
        (<PassingStats>stats).sackYards = item.sack_yds;
        (<PassingStats>stats).interceptions = item.pass_int;
        (<PassingStats>stats).detmer = item.detmer;
        (<PassingStats>stats).detmerPerGame = item.detmergame;
    
        (<PassingStats>stats).completionPctRank = item.comppct_rank;
        (<PassingStats>stats).detmerRank = item.detmer_rank;
        (<PassingStats>stats).detmerPerGameRank = item.detmergame_rank;

        (<PassingStats>stats).dropbacks = item.dropbacks;
        (<PassingStats>stats).sackAdjustedYards = item.sack_adj_yards;
        (<PassingStats>stats).yardsPerDropback = item.yardsdropback;
        (<PassingStats>stats).sackAdjustedYardsRank = item.sack_adj_yards_rank;
        (<PassingStats>stats).yardsPerDropbackRank = item.yardsdropback_rank;

    } else if (type === SummaryType.Rushing) {
        (<RushingStats>stats).touchdowns = item.rushing_td;
        (<RushingStats>stats).fumbles = item.fumbles;
    } else if (type === SummaryType.Receiving) {
        (<ReceivingStats>stats).catches = item.comp,
        (<ReceivingStats>stats).targets = item.targets,
        (<ReceivingStats>stats).catchPct = item.catchpct,
        (<ReceivingStats>stats).touchdowns = item.passing_td,
        (<ReceivingStats>stats).fumbles = item.fumbles,
    
        (<ReceivingStats>stats).catchPctRank = item.catchpct_rank
    }
    Object.keys(stats).forEach(key => {
        stats[key] = (stats[key] == "NA") ? null : parseFloat(stats[key])
    })
    return <PlayerStatistics>stats;
}

function parseSummary(content: any[], type: SummaryType): Summary[] {
    const result: Summary[] = [];
    if (type === SummaryType.Overall) {
        for (const item of content) {
            let team: any = {
                season: item.season,
                teamId: item.team_id,
                team: item.pos_team,
                offensive: {
                    overall: {
                        totalPlays: item.plays_off,
                        playsPerGame: item.playsgame_off,
                        totalEPA: item.TEPA_off,
                        epaPerPlay: item.EPAplay_off,
                        adjEpaPerPlay: item.adj_off_epa,
                        epaPerGame: item.EPAgame_off,
                        successRate: item.success_off,
                        startingFP: item.start_position_off,

                        yards: item.yards_off,
                        yardsPerPlay: item.yardsplay_off,
                        yardsPerGame: item.yardsgame_off,
                        
                        playsPerGameRank: item.playsgame_off_rank,
                        totalEPARank: item.TEPA_off_rank,
                        epaPerPlayRank: item.EPAplay_off_rank,
                        adjEpaPerPlayRank: item.adj_off_epa_rank,
                        epaPerGameRank: item.EPAgame_off_rank,
                        successRateRank: item.success_off_rank,
                        startingFPRank: item.start_position_off_rank,

                        yardsRank: item.yards_off_rank,
                        yardsPerPlayRank: item.yardsplay_off_rank,
                        yardsPerGameRank: item.yardsgame_off_rank,

                        totalAvailableYards: item.total_available_yards_off,
                        totalGainedYards: item.total_gained_yards_off,
                        availableYardsPct: item.available_yards_pct_off,

                        totalAvailableYardsRank: item.total_available_yards_off_rank,
                        totalGainedYardsRank: item.total_gained_yards_off_rank,
                        availableYardsPctRank: item.available_yards_pct_off_rank,

                        stuffedPlayRate: item.play_stuffed_off,
                        stuffedPlayRateRank: item.play_stuffed_off_rank,
                        redZoneSuccessRate: item.red_zone_success_off,
                        redZoneSuccessRateRank: item.red_zone_success_off_rank,
                        thirdDownSuccessRate: item.third_down_success_off,
                        thirdDownSuccessRateRank: item.third_down_success_off_rank,

                        explosiveRate: item.explosive_off,
                        explosiveRateRank: item.explosive_off_rank,
                        havocRate: item.havoc_off,
                        havocRateRank: item.havoc_off_rank,

                        epaPerDrive: item.EPAdrive_off,
                        epaPerDriveRank: item.EPAdrive_off_rank,
                        lateDownSuccessRate: item.late_down_success_off,
                        lateDownSuccessRateRank: item.late_down_success_off_rank,
                        earlyDownEPAPerPlay: item.early_down_EPA_off,
                        earlyDownEPAPerPlayRank: item.early_down_EPA_off_rank,
                        thirdDownDistance: item.third_down_distance_off,
                        thirdDownDistanceRank: item.third_down_distance_off_rank,

                        nonExplosiveEpaPerPlay: item.nonExplosiveEpaPerPlay_off,
                        nonExplosiveEpaPerPlayRank: item.nonExplosiveEpaPerPlay_off_rank,
                        lineYards: item.line_yards_off,
                        lineYardsRank: item.line_yards_off_rank,
                        opportunityRate: item.opportunity_rate_off,
                        opportunityRateRank: item.opportunity_rate_off_rank,
                    },
                    passing: {
                        totalPlays: item.plays_off_pass,
                        playsPerGame: item.playsgame_off_pass,
                        totalEPA: item.TEPA_off_pass,
                        epaPerPlay: item.EPAplay_off_pass,
                        adjEpaPerPlay: item.adj_off_epa_pass,
                        epaPerGame: item.EPAgame_off_pass,
                        successRate: item.success_off_pass,
                        startingFP: item.start_position_off_pass,
                        
                        playsPerGameRank: item.playsgame_off_pass_rank,
                        totalEPARank: item.TEPA_off_pass_rank,
                        epaPerPlayRank: item.EPAplay_off_pass_rank,
                        adjEpaPerPlayRank: item.adj_off_epa_pass_rank,
                        epaPerGameRank: item.EPAgame_off_pass_rank,
                        successRateRank: item.success_off_pass_rank,
                        startingFPRank: item.start_position_off_pass_rank,

                        yards: item.yards_off_pass,
                        yardsPerPlay: item.yardsplay_off_pass,
                        yardsPerGame: item.yardsgame_off_pass,

                        yardsRank: item.yards_off_pass_rank,
                        yardsPerPlayRank: item.yardsplay_off_pass_rank,
                        yardsPerGameRank: item.yardsgame_off_pass_rank,

                        stuffedPlayRate: item.play_stuffed_off_pass,
                        stuffedPlayRateRank: item.play_stuffed_off_pass_rank,
                        redZoneSuccessRate: item.red_zone_success_off_pass,
                        redZoneSuccessRateRank: item.red_zone_success_off_pass_rank,
                        thirdDownSuccessRate: item.third_down_success_off_pass,
                        thirdDownSuccessRateRank: item.third_down_success_off_pass_rank,

                        explosiveRate: item.explosive_off_pass,
                        explosiveRateRank: item.explosive_off_pass_rank,
                        havocRate: item.havoc_off_pass,
                        havocRateRank: item.havoc_off_pass_rank,

                        epaPerDrive: item.EPAdrive_off_pass,
                        epaPerDriveRank: item.EPAdrive_off_pass_rank,
                        lateDownSuccessRate: item.late_down_success_off_pass,
                        lateDownSuccessRateRank: item.late_down_success_off_pass_rank,
                        earlyDownEPAPerPlay: item.early_down_EPA_off_pass,
                        earlyDownEPAPerPlayRank: item.early_down_EPA_off_pass_rank,
                        thirdDownDistance: item.third_down_distance_off_pass,
                        thirdDownDistanceRank: item.third_down_distance_off_pass_rank,


                        nonExplosiveEpaPerPlay: item.nonExplosiveEpaPerPlay_off_pass,
                        nonExplosiveEpaPerPlayRank: item.nonExplosiveEpaPerPlay_off_pass_rank,
                        lineYards: item.line_yards_off_pass,
                        lineYardsRank: item.line_yards_off_pass_rank,
                        opportunityRate: item.opportunity_rate_off_pass,
                        opportunityRateRank: item.opportunity_rate_off_pass_rank,
                    },
                    rushing: {
                        totalPlays: item.plays_off_rush,
                        playsPerGame: item.playsgame_off_rush,
                        totalEPA: item.TEPA_off_rush,
                        epaPerPlay: item.EPAplay_off_rush,
                        adjEpaPerPlay: item.adj_off_epa_rush,
                        epaPerGame: item.EPAgame_off_rush,
                        successRate: item.success_off_rush,
                        startingFP: item.start_position_off_rush,
                        
                        playsPerGameRank: item.playsgame_off_rush_rank,
                        totalEPARank: item.TEPA_off_rush_rank,
                        epaPerPlayRank: item.EPAplay_off_rush_rank,
                        adjEpaPerPlayRank: item.adj_off_epa_rush_rank,
                        epaPerGameRank: item.EPAgame_off_rush_rank,
                        successRateRank: item.success_off_rush_rank,
                        startingFPRank: item.start_position_off_rush_rank,

                        yards: item.yards_off_rush,
                        yardsPerPlay: item.yardsplay_off_rush,
                        yardsPerGame: item.yardsgame_off_rush,

                        yardsRank: item.yards_off_rush_rank,
                        yardsPerPlayRank: item.yardsplay_off_rush_rank,
                        yardsPerGameRank: item.yardsgame_off_rush_rank,

                        stuffedPlayRate: item.play_stuffed_off_rush,
                        stuffedPlayRateRank: item.play_stuffed_off_rush_rank,
                        redZoneSuccessRate: item.red_zone_success_off_rush,
                        redZoneSuccessRateRank: item.red_zone_success_off_rush_rank,
                        thirdDownSuccessRate: item.third_down_success_off_rush,
                        thirdDownSuccessRateRank: item.third_down_success_off_rush_rank,

                        explosiveRate: item.explosive_off_rush,
                        explosiveRateRank: item.explosive_off_rush_rank,
                        havocRate: item.havoc_off_rush,
                        havocRateRank: item.havoc_off_rush_rank,

                        epaPerDrive: item.EPAdrive_off_rush,
                        epaPerDriveRank: item.EPAdrive_off_rush_rank,
                        lateDownSuccessRate: item.late_down_success_off_rush,
                        lateDownSuccessRateRank: item.late_down_success_off_rush_rank,
                        earlyDownEPAPerPlay: item.early_down_EPA_off_rush,
                        earlyDownEPAPerPlayRank: item.early_down_EPA_off_rush_rank,
                        thirdDownDistance: item.third_down_distance_off_rush,
                        thirdDownDistanceRank: item.third_down_distance_off_rush_rank,

                        nonExplosiveEpaPerPlay: item.nonExplosiveEpaPerPlay_off_rush,
                        nonExplosiveEpaPerPlayRank: item.nonExplosiveEpaPerPlay_off_rush_rank,
                        lineYards: item.line_yards_off_rush,
                        lineYardsRank: item.line_yards_off_rush_rank,
                        opportunityRate: item.opportunity_rate_off_rush,
                        opportunityRateRank: item.opportunity_rate_off_rush_rank,
                    }
                },
                defensive: {
                    overall: {
                        totalPlays: item.plays_def,
                        playsPerGame: item.playsgame_def,
                        totalEPA: item.TEPA_def,
                        epaPerPlay: item.EPAplay_def,
                        adjEpaPerPlay: item.adj_def_epa,
                        epaPerGame: item.EPAgame_def,
                        successRate: item.success_def,
                        startingFP: item.start_position_def,
                        
                        playsPerGameRank: item.playsgame_def_rank,
                        totalEPARank: item.TEPA_def_rank,
                        epaPerPlayRank: item.EPAplay_def_rank,
                        adjEpaPerPlayRank: item.adj_def_epa_rank,
                        epaPerGameRank: item.EPAgame_def_rank,
                        successRateRank: item.success_def_rank,
                        startingFPRank: item.start_position_def_rank,

                        yards: item.yards_def,
                        yardsPerPlay: item.yardsplay_def,
                        yardsPerGame: item.yardsgame_def,

                        yardsRank: item.yards_def_rank,
                        yardsPerPlayRank: item.yardsplay_def_rank,
                        yardsPerGameRank: item.yardsgame_def_rank,

                        totalAvailableYards: item.total_available_yards_def,
                        totalGainedYards: item.total_gained_yards_def,
                        availableYardsPct: item.available_yards_pct_def,

                        totalAvailableYardsRank: item.total_available_yards_def_rank,
                        totalGainedYardsRank: item.total_gained_yards_def_rank,
                        availableYardsPctRank: item.available_yards_pct_def_rank,

                        stuffedPlayRate: item.play_stuffed_def,
                        stuffedPlayRateRank: item.play_stuffed_def_rank,
                        redZoneSuccessRate: item.red_zone_success_def,
                        redZoneSuccessRateRank: item.red_zone_success_def_rank,
                        thirdDownSuccessRate: item.third_down_success_def,
                        thirdDownSuccessRateRank: item.third_down_success_def_rank,

                        explosiveRate: item.explosive_def,
                        explosiveRateRank: item.explosive_def_rank,
                        havocRate: item.havoc_def,
                        havocRateRank: item.havoc_def_rank,

                        epaPerDrive: item.EPAdrive_def,
                        epaPerDriveRank: item.EPAdrive_def_rank,
                        lateDownSuccessRate: item.late_down_success_def,
                        lateDownSuccessRateRank: item.late_down_success_def_rank,
                        earlyDownEPAPerPlay: item.early_down_EPA_def,
                        earlyDownEPAPerPlayRank: item.early_down_EPA_def_rank,
                        thirdDownDistance: item.third_down_distance_def,
                        thirdDownDistanceRank: item.third_down_distance_def_rank,

                        nonExplosiveEpaPerPlay: item.nonExplosiveEpaPerPlay_def,
                        nonExplosiveEpaPerPlayRank: item.nonExplosiveEpaPerPlay_def_rank,
                        lineYards: item.line_yards_def,
                        lineYardsRank: item.line_yards_def_rank,
                        opportunityRate: item.opportunity_rate_def,
                        opportunityRateRank: item.opportunity_rate_def_rank,
                    },
                    passing: {
                        totalPlays: item.plays_def_pass,
                        playsPerGame: item.playsgame_def_pass,
                        totalEPA: item.TEPA_def_pass,
                        epaPerPlay: item.EPAplay_def_pass,
                        adjEpaPerPlay: item.adj_def_epa_pass,
                        epaPerGame: item.EPAgame_def_pass,
                        successRate: item.success_def_pass,
                        startingFP: item.start_position_def_pass,
                        
                        playsPerGameRank: item.playsgame_def_pass_rank,
                        totalEPARank: item.TEPA_def_pass_rank,
                        epaPerPlayRank: item.EPAplay_def_pass_rank,
                        adjEpaPerPlayRank: item.adj_def_epa_pass_rank,
                        epaPerGameRank: item.EPAgame_def_pass_rank,
                        successRateRank: item.success_def_pass_rank,
                        startingFPRank: item.start_position_def_pass_rank,

                        yards: item.yards_def_pass,
                        yardsPerPlay: item.yardsplay_def_pass,
                        yardsPerGame: item.yardsgame_def_pass,

                        yardsRank: item.yards_def_pass_rank,
                        yardsPerPlayRank: item.yardsplay_def_pass_rank,
                        yardsPerGameRank: item.yardsgame_def_pass_rank,

                        stuffedPlayRate: item.play_stuffed_def_pass,
                        stuffedPlayRateRank: item.play_stuffed_def_pass_rank,
                        redZoneSuccessRate: item.red_zone_success_def_pass,
                        redZoneSuccessRateRank: item.red_zone_success_def_pass_rank,
                        thirdDownSuccessRate: item.third_down_success_def_pass,
                        thirdDownSuccessRateRank: item.third_down_success_def_pass_rank,

                        explosiveRate: item.explosive_def_pass,
                        explosiveRateRank: item.explosive_def_pass_rank,
                        havocRate: item.havoc_def_pass,
                        havocRateRank: item.havoc_def_pass_rank,

                        epaPerDrive: item.EPAdrive_def_pass,
                        epaPerDriveRank: item.EPAdrive_def_pass_rank,
                        lateDownSuccessRate: item.late_down_success_def_pass,
                        lateDownSuccessRateRank: item.late_down_success_def_pass_rank,
                        earlyDownEPAPerPlay: item.early_down_EPA_def_pass,
                        earlyDownEPAPerPlayRank: item.early_down_EPA_def_pass_rank,

                        thirdDownDistance: item.third_down_distance_def_pass,
                        thirdDownDistanceRank: item.third_down_distance_def_pass_rank,

                        nonExplosiveEpaPerPlay: item.nonExplosiveEpaPerPlay_def_pass,
                        nonExplosiveEpaPerPlayRank: item.nonExplosiveEpaPerPlay_def_pass_rank,
                        lineYards: item.line_yards_def_pass,
                        lineYardsRank: item.line_yards_def_pass_rank,
                        opportunityRate: item.opportunity_rate_def_pass,
                        opportunityRateRank: item.opportunity_rate_def_pass_rank,
                    },
                    rushing: {
                        totalPlays: item.plays_def_rush,
                        playsPerGame: item.playsgame_def_rush,
                        totalEPA: item.TEPA_def_rush,
                        epaPerPlay: item.EPAplay_def_rush,
                        adjEpaPerPlay: item.adj_def_epa_rush_rank,
                        epaPerGame: item.EPAgame_def_rush,
                        successRate: item.success_def_rush,
                        startingFP: item.start_position_def_rush,
                        
                        playsPerGameRank: item.playsgame_def_rush_rank,
                        totalEPARank: item.TEPA_def_rush_rank,
                        epaPerPlayRank: item.EPAplay_def_rush_rank,
                        adjEpaPerPlayRank: item.adj_def_epa_rush_rank,
                        epaPerGameRank: item.EPAgame_def_rush_rank,
                        successRateRank: item.success_def_rush_rank,
                        startingFPRank: item.start_position_def_rush_rank,

                        yards: item.yards_def_rush,
                        yardsPerPlay: item.yardsplay_def_rush,
                        yardsPerGame: item.yardsgame_def_rush,

                        yardsRank: item.yards_def_rush_rank,
                        yardsPerPlayRank: item.yardsplay_def_rush_rank,
                        yardsPerGameRank: item.yardsgame_def_rush_rank,

                        stuffedPlayRate: item.play_stuffed_def_rush,
                        stuffedPlayRateRank: item.play_stuffed_def_rush_rank,
                        redZoneSuccessRate: item.red_zone_success_def_rush,
                        redZoneSuccessRateRank: item.red_zone_success_def_rush_rank,
                        thirdDownSuccessRate: item.third_down_success_def_rush,
                        thirdDownSuccessRateRank: item.third_down_success_def_rush_rank,

                        explosiveRate: item.explosive_def_rush,
                        explosiveRateRank: item.explosive_def_rush_rank,
                        havocRate: item.havoc_def_rush,
                        havocRateRank: item.havoc_def_rush_rank,

                        epaPerDrive: item.EPAdrive_def_rush,
                        epaPerDriveRank: item.EPAdrive_def_rush_rank,
                        lateDownSuccessRate: item.late_down_success_def_rush,
                        lateDownSuccessRateRank: item.late_down_success_def_rush_rank,
                        earlyDownEPAPerPlay: item.early_down_EPA_def_rush,
                        earlyDownEPAPerPlayRank: item.early_down_EPA_def_rush_rank,

                        thirdDownDistance: item.third_down_distance_def_rush,
                        thirdDownDistanceRank: item.third_down_distance_def_rush_rank,

                        nonExplosiveEpaPerPlay: item.nonExplosiveEpaPerPlay_def_rush,
                        nonExplosiveEpaPerPlayRank: item.nonExplosiveEpaPerPlay_def_rush_rank,
                        lineYards: item.line_yards_def_rush,
                        lineYardsRank: item.line_yards_def_rush_rank,
                        opportunityRate: item.opportunity_rate_def_rush,
                        opportunityRateRank: item.opportunity_rate_def_rush_rank,
                    }
                },
                differential: {
                    overall: {
                        totalEPA: item.TEPA_margin,
                        epaPerPlay: item.EPAplay_margin,
                        adjEpaPerPlay: item.net_adj_epa,
                        epaPerGame: item.EPAgame_margin,
                        successRate: item.success_margin,
                        startingFP: item.start_position_margin,

                        totalEPARank: item.TEPA_margin_rank,
                        epaPerPlayRank: item.EPAplay_margin_rank,
                        adjEpaPerPlayRank: item.net_adj_epa_rank,
                        epaPerGameRank: item.EPAgame_margin_rank,
                        successRateRank: item.success_margin_rank,
                        startingFPRank: item.start_position_margin_rank,

                        yards: item.yards_margin,
                        yardsPerPlay: item.yardsplay_margin,
                        yardsPerGame: item.yardsgame_margin,

                        yardsRank: item.yards_margin_rank,
                        yardsPerPlayRank: item.yardsplay_margin_rank,
                        yardsPerGameRank: item.yardsgame_margin_rank,

                        totalAvailableYards: item.total_available_yards_margin,
                        totalGainedYards: item.total_gained_yards_margin,
                        availableYardsPct: item.available_yards_pct_margin,

                        totalAvailableYardsRank: item.total_available_yards_margin_rank,
                        totalGainedYardsRank: item.total_gained_yards_margin_rank,
                        availableYardsPctRank: item.available_yards_pct_margin_rank,

                        epaPerDrive: item.EPAdrive_margin,
                        epaPerDriveRank: item.EPAdrive_margin_rank
                    },
                    passing: {
                        totalEPA: item.TEPA_margin_pass,
                        epaPerPlay: item.EPAplay_margin_pass,
                        adjEpaPerPlay: item.net_adj_epa_pass,
                        epaPerGame: item.EPAgame_margin_pass,
                        successRate: item.success_margin_pass,
                        startingFP: item.start_position_margin_pass,
                        
                        totalEPARank: item.TEPA_margin_pass_rank,
                        epaPerPlayRank: item.EPAplay_margin_pass_rank,
                        adjEpaPerPlayRank: item.net_adj_epa_rank,
                        epaPerGameRank: item.EPAgame_margin_pass_rank,
                        successRateRank: item.success_margin_pass_rank,
                        startingFPRank: item.start_position_margin_pass_rank,

                        yards: item.yards_margin_pass,
                        yardsPerPlay: item.yardsplay_margin_pass,
                        yardsPerGame: item.yardsgame_margin_pass,

                        yardsRank: item.yards_margin_pass_rank,
                        yardsPerPlayRank: item.yardsplay_margin_pass_rank,
                        yardsPerGameRank: item.yardsgame_margin_pass_rank,

                        epaPerDrive: item.EPAdrive_margin_pass,
                        epaPerDriveRank: item.EPAdrive_margin_pass_rank
                    },
                    rushing: {
                        totalEPA: item.TEPA_margin_rush,
                        epaPerPlay: item.EPAplay_margin_rush,
                        adjEpaPerPlay: item.net_adj_epa_rush,
                        epaPerGame: item.EPAgame_margin_rush,
                        successRate: item.success_margin_rush,
                        startingFP: item.start_position_margin_rush,
                        
                        totalEPARank: item.TEPA_margin_rush_rank,
                        epaPerPlayRank: item.EPAplay_margin_rush_rank,
                        adjEpaPerPlayRank: item.net_adj_epa_rank,
                        epaPerGameRank: item.EPAgame_margin_rush_rank,
                        successRateRank: item.success_margin_rush_rank,
                        startingFPRank: item.start_position_margin_rush_rank,

                        yards: item.yards_margin_rush,
                        yardsPerPlay: item.yardsplay_margin_rush,
                        yardsPerGame: item.yardsgame_margin_rush,

                        yardsRank: item.yards_margin_rush_rank,
                        yardsPerPlayRank: item.yardsplay_margin_rush_rank,
                        yardsPerGameRank: item.yardsgame_margin_rush_rank,

                        epaPerDrive: item.EPAdrive_margin_rush,
                        epaPerDriveRank: item.EPAdrive_margin_rush_rank
                    }
                }
            };
            for (const key of Object.keys(team)) {
                let subDict = team[key]
                if (typeof subDict === 'object' && !Array.isArray(subDict) && subDict !== null) {
                    for (const subKey of Object.keys(subDict)) {
                        let subSubDict = team[key][subKey]
                        if (typeof subSubDict === 'object' && !Array.isArray(subSubDict) && subSubDict !== null) {
                            Object.keys(subSubDict).forEach(subSubKey => {
                                team[key][subKey][subSubKey] = (team[key][subKey][subSubKey] == "NA") ? null : team[key][subKey][subSubKey]; //parseFloat(team[key][subKey][subSubKey]) // maintain compat
                            })
                        }
                    }
                }
            }
            result.push(<Summary>team);
        }
    } else {
        for (const item of content) {
            let summ: PlayerSummary = {
                season: item.season,
                teamId: item.team_id,
                team: item.pos_team,
                name: parseName(item, type),
                playerId: item.player_id,
                advanced: parseAdvanced(item),
                statistics: parseStats(item, type)
            }

            result.push(summ);
        }
    }
    return result;
}

async function retrieveSummaryData(params: SummaryRequest): Promise<Summary[]> {
    let fileName: string = `./data/${params.year}/`
    if (params.team) {
        fileName += `${params.team}/`
    }
    fileName += `${params.type ?? SummaryType.Overall}.csv`

    console.log(`Looking for data at file path: ${fileName}`)
    const content = await csvtojson().fromFile(fileName);
    
    return parseSummary(content, params.type ?? SummaryType.Overall);
}

async function retrievePercentiles(year: number): Promise<Percentile[]> {
    const fileName: string = `./data/${year}/percentiles.csv`
    console.log(`Looking for percentiles data at file path: ${fileName}`)
    const content = await csvtojson().fromFile(fileName);
    
    return parsePercentiles(year, content);
}

function parsePercentiles(year: number, content: any[]): Percentile[] {
    const result: Percentile[] = [];
    for (const item of content) {
        const pctl: Percentile = {
            season: year,
            pctile: parseFloat(item.pctile),
            gei: parseFloat(item.GEI),
            epaPerPlay: parseFloat(item.EPAplay), // EPAplay
            successRate: parseFloat(item.success), // success
            yardsPerPlay: parseFloat(item.yardsplay), // yardsplay
            epaPerDropback: parseFloat(item.EPAdropback), // EPAdropback
            epaPerRush: parseFloat(item.EPArush), // EPArush
            yardsPerDropback: parseFloat(item.yardsdropback), // yardsdropback
        
            explosivePlayRate: parseFloat(item.explosive), //explosive
            thirdDownSuccessRate: parseFloat(item.third_down_success), // third_down_success
            redZoneSuccessRate: parseFloat(item.red_zone_success), // red_zone_success
        
            playStuffedRate: parseFloat(item.play_stuffed), // play_stuffed
            havocRate: parseFloat(item.havoc), //havoc
        };
        result.push(pctl);
    }
    result.sort((a, b): number => {
        return a.pctile - b.pctile;
    });
    return result;
}
 
// Initialize the express engine
const app: express.Application = express();
app.use(morgan('[summaries] :remote-addr - :remote-user [:date[clf]] ":method :url HTTP/:http-version" :status :res[content-length]'));

app.use(bodyParser.urlencoded({ extended: true }));
app.get('/', (_req, _res) => {
    _res.status(400).json({
        error: 'Invalid endpoint'
    })
});

app.get('/health', (_req, _res) => {
    _res.status(200).json({
        status: 'ok'
    })
});

app.get('/percentiles/:year', async (req, res, next) => {
    console.debug('Received GET percentiles request with params ' + JSON.stringify(req.params));
    try {
        const content = await retrievePercentiles(parseInt(req.params.year));
        return res.status(200).json({
            results: content
        });
    } catch (err) {
        console.log(err);
        return res.status(404).json({
            error: 'no data found'
        });
    }
})

app.post('/', async (req, res, next) => {
    const body = req.body;
    if (!body) {
        return res.status(400).json({
            error: 'No POST body sent'
        })
    }
    if (!body.year) {
        return res.status(400).json({
            error: 'Did not provide required param `year`'
        })    
    }

    const parsedBody: SummaryRequest = body;
    console.debug('Received POST request with params ' + JSON.stringify(parsedBody));
    try {
        const content = await retrieveSummaryData(parsedBody);
        // console.debug('Found content ' + JSON.stringify(content));
        return res.status(200).json({
            results: content
        });
    } catch (err) {
        console.log(err);
        return res.status(404).json({
            error: 'no data found'
        });
    }
});

app.get("/updated", async (req, res, next) => {
    try {
        return res.status(200).json({
            last_updated: lastUpdated
        });
    } catch (err) {
        console.log(err);
        return res.status(404).json({
            error: 'no data found'
        });
    }
})

// Server setup
const port: string = process.env.PORT || '3000';

console.log(`server starting at port ${port}`);
app.listen(port, async () => {
    console.log(`server started at port ${port}`);
});