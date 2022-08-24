
// Import the express in typescript file
import express from 'express';
import process from 'process';
import csvtojson from 'csvtojson';
import { TeamSummary } from './interfaces/overall-summary';
import { SummaryType } from './enums/summary-types';
import { Summary, SummaryRequest } from './interfaces/request';
import { PassingStats, PlayerStatistics, PlayerSummary, ReceivingStats, RushingStats } from './interfaces/player-summary';
import bodyParser from 'body-parser';
import morgan from 'morgan';

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
        yardsPerGameRank: item.yardsgame_Rank
    }
    if (type === SummaryType.Passing) {
        (<PassingStats>stats).completions = item.comp;
        (<PassingStats>stats).attempts = item.att;
        (<PassingStats>stats).completionPct = item.comppct;
        (<PassingStats>stats).touchdowns = item.passing_td;
        (<PassingStats>stats).sacks = item.sacked;
        (<PassingStats>stats).sackYards = item.sack_yds == "NA" ? "0" : item.sack_yds;
        (<PassingStats>stats).interceptions = item.pass_int;
        (<PassingStats>stats).detmer = item.detmer;
        (<PassingStats>stats).detmerPerGame = item.detmergame;
    
        (<PassingStats>stats).completionPctRank = item.comppc_rank;
        (<PassingStats>stats).detmerRank = item.detmer_rank;
        (<PassingStats>stats).detmerPerGameRank = item.detmergame_rank;
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
    return <PlayerStatistics>stats;
}

function parseSummary(content: any[], type: SummaryType): Summary[] {
    const result: Summary[] = [];
    if (type === SummaryType.Overall) {
        for (const item of content) {
            const team: TeamSummary = {
                season: item.season,
                teamId: item.team_id,
                team: item.pos_team,
                offensive: {
                    overall: {
                        totalPlays: item.plays_off,
                        playsPerGame: item.playsgame_off,
                        totalEPA: item.TEPA_off,
                        epaPerPlay: item.EPAplay_off,
                        epaPerGame: item.EPAgame_off,
                        successRate: item.success_off,
                        startingFP: item.start_position_off,
                        
                        playsPerGameRank: item.playsgame_off_rank,
                        totalEPARank: item.TEPA_off_rank,
                        epaPerPlayRank: item.EPAplay_off_rank,
                        epaPerGameRank: item.EPAgame_off_rank,
                        successRateRank: item.success_off_rank,
                        startingFPRank: item.start_position_off_rank
                    },
                    passing: {
                        totalPlays: item.plays_off_pass,
                        playsPerGame: item.playsgame_off_pass,
                        totalEPA: item.TEPA_off_pass,
                        epaPerPlay: item.EPAplay_off_pass,
                        epaPerGame: item.EPAgame_off_pass,
                        successRate: item.success_off_pass,
                        startingFP: item.start_position_off_pass,
                        
                        playsPerGameRank: item.playsgame_off_pass_rank,
                        totalEPARank: item.TEPA_off_pass_rank,
                        epaPerPlayRank: item.EPAplay_off_pass_rank,
                        epaPerGameRank: item.EPAgame_off_pass_rank,
                        successRateRank: item.success_off_pass_rank,
                        startingFPRank: item.start_position_off_pass_rank
                    },
                    rushing: {
                        totalPlays: item.plays_off_rush,
                        playsPerGame: item.playsgame_off_rush,
                        totalEPA: item.TEPA_off_rush,
                        epaPerPlay: item.EPAplay_off_rush,
                        epaPerGame: item.EPAgame_off_rush,
                        successRate: item.success_off_rush,
                        startingFP: item.start_position_off_rush,
                        
                        playsPerGameRank: item.playsgame_off_rush_rank,
                        totalEPARank: item.TEPA_off_rush_rank,
                        epaPerPlayRank: item.EPAplay_off_rush_rank,
                        epaPerGameRank: item.EPAgame_off_rush_rank,
                        successRateRank: item.success_off_rush_rank,
                        startingFPRank: item.start_position_off_rush_rank
                    }
                },
                defensive: {
                    overall: {
                        totalPlays: item.plays_def,
                        playsPerGame: item.playsgame_def,
                        totalEPA: item.TEPA_def,
                        epaPerPlay: item.EPAplay_def,
                        epaPerGame: item.EPAgame_def,
                        successRate: item.success_def,
                        startingFP: item.start_position_def,
                        
                        playsPerGameRank: item.playsgame_def_rank,
                        totalEPARank: item.TEPA_def_rank,
                        epaPerPlayRank: item.EPAplay_def_rank,
                        epaPerGameRank: item.EPAgame_def_rank,
                        successRateRank: item.success_def_rank,
                        startingFPRank: item.start_position_def_rank
                    },
                    passing: {
                        totalPlays: item.plays_def_pass,
                        playsPerGame: item.playsgame_def_pass,
                        totalEPA: item.TEPA_def_pass,
                        epaPerPlay: item.EPAplay_def_pass,
                        epaPerGame: item.EPAgame_def_pass,
                        successRate: item.success_def_pass,
                        startingFP: item.start_position_def_pass,
                        
                        playsPerGameRank: item.playsgame_def_pass_rank,
                        totalEPARank: item.TEPA_def_pass_rank,
                        epaPerPlayRank: item.EPAplay_def_pass_rank,
                        epaPerGameRank: item.EPAgame_def_pass_rank,
                        successRateRank: item.success_def_pass_rank,
                        startingFPRank: item.start_position_def_pass_rank
                    },
                    rushing: {
                        totalPlays: item.plays_def_rush,
                        playsPerGame: item.playsgame_def_rush,
                        totalEPA: item.TEPA_def_rush,
                        epaPerPlay: item.EPAplay_def_rush,
                        epaPerGame: item.EPAgame_def_rush,
                        successRate: item.success_def_rush,
                        startingFP: item.start_position_def_rush,
                        
                        playsPerGameRank: item.playsgame_def_rush_rank,
                        totalEPARank: item.TEPA_def_rush_rank,
                        epaPerPlayRank: item.EPAplay_def_rush_rank,
                        epaPerGameRank: item.EPAgame_def_rush_rank,
                        successRateRank: item.success_def_rush_rank,
                        startingFPRank: item.start_position_def_rush_rank
                    }
                },
                differential: {
                    overall: {
                        totalEPA: item.TEPA_margin,
                        epaPerPlay: item.EPAplay_margin,
                        epaPerGame: item.EPAgame_margin,
                        successRate: item.success_margin,
                        startingFP: item.start_position_margin,

                        totalEPARank: item.TEPA_margin_rank,
                        epaPerPlayRank: item.EPAplay_margin_rank,
                        epaPerGameRank: item.EPAgame_margin_rank,
                        successRateRank: item.success_margin_rank,
                        startingFPRank: item.start_position_margin_rank
                    },
                    passing: {
                        totalEPA: item.TEPA_margin_pass,
                        epaPerPlay: item.EPAplay_margin_pass,
                        epaPerGame: item.EPAgame_margin_pass,
                        successRate: item.success_margin_pass,
                        startingFP: item.start_position_margin_pass,
                        
                        totalEPARank: item.TEPA_margin_pass_rank,
                        epaPerPlayRank: item.EPAplay_margin_pass_rank,
                        epaPerGameRank: item.EPAgame_margin_pass_rank,
                        successRateRank: item.success_margin_pass_rank,
                        startingFPRank: item.start_position_margin_pass_rank
                    },
                    rushing: {
                        totalEPA: item.TEPA_margin_rush,
                        epaPerPlay: item.EPAplay_margin_rush,
                        epaPerGame: item.EPAgame_margin_rush,
                        successRate: item.success_margin_rush,
                        startingFP: item.start_position_margin_rush,
                        
                        totalEPARank: item.TEPA_margin_rush_rank,
                        epaPerPlayRank: item.EPAplay_margin_rush_rank,
                        epaPerGameRank: item.EPAgame_margin_rush_rank,
                        successRateRank: item.success_margin_rush_rank,
                        startingFPRank: item.start_position_margin_rush_rank
                    }
                }
            };
            result.push(team);
        }
    } else {
        for (const item of content) {
            const summ: PlayerSummary = {
                season: item.season,
                teamId: item.team_id,
                team: item.pos_team,
                name: parseName(item, type),
                advanced: {
                    totalEPA: item.TEPA,
                    epaPerPlay: item.EPAplay,
                    epaPerGame: item.EPAgame,
                    successRate: item.success,
                
                    totalEPARank: item.TEPA_rank,
                    epaPerPlayRank: item.EPAplay_rank,
                    epaPerGameRank: item.EPAgame_rank,
                    successRateRank: item.success_rank
                },
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
    const content = await retrieveSummaryData(parsedBody);
    console.debug('Found content ' + JSON.stringify(content));
    return res.status(200).json({
        results: content
    });
});

// Server setup
const port: string = process.env.PORT || '3000';

console.log(`server starting at port ${port}`);
app.listen(port, async () => {
    console.log(`server started at port ${port}`);
});