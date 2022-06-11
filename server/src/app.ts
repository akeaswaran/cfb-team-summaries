
// Import the express in typescript file
import express from 'express';
import process from 'process';
import csvtojson from 'csvtojson';
import { Summary } from './interfaces/summary';

async function retrieveSummaryData(year: number, team?: string, type = 'overall'): Promise<Summary[]> {
    let fileName: string = `./data/${year}/`
    if (team) {
        fileName += `${team}/`
    }
    fileName += `${type}.csv`

    console.log(`Looking for data at file path: ${fileName}`)
    const content = await csvtojson().fromFile(fileName);
    const result: Summary[] = [];
    for (const item of content) {
        result.push({
            season: item.season,
            teamId: item.team_id,
            team: item.pos_team,
            offensive: {
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
            defensive: {
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
            differential: {
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
            }
        });
    }
    return result;
}
 
// Initialize the express engine
const app: express.Application = express();

app.get('/', (_req, _res) => {
    _res.status(400).json({
        error: 'Invalid endpoint'
    })
});

app.get('/year/:year', async (req, res) => {
    const year: number = parseInt(req.params.year);
    try {
        const summ: Summary[] = await retrieveSummaryData(year);
        return res.status(200).json({
            results: summ
        });
    } catch (e) {
        console.error(e);
        return res.status(400).json({
            error: `Data not found for inputs: year - ${year}`
        });
    }
});

app.get('/year/:year/team/:team', async (req, res) => {
    const year: number = parseInt(req.params.year);
    const team: string = req.params.team;
    try {
        const summ: Summary[] = await retrieveSummaryData(year, team);
        return res.status(200).json({
            results: summ
        });
    } catch (e) {
        console.error(e);
        return res.status(400).json({
            error: `Data not found for inputs: year - ${year}, team - ${team}`
        });
    }
})

// Server setup
const port: string = process.env.PORT || '3000';

console.log(`server starting at port ${port}`);
app.listen(port, async () => {
    console.log(`server started at port ${port}`);
});