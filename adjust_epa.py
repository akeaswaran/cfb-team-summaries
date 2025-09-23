# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "numpy",
#     "pandas",
#     "pyarrow",
#     "scikit-learn",
# ]
# ///

import pandas as pd
import numpy as np
from sklearn import linear_model

def retrieve_pbp(year: int) -> pd.DataFrame:
    print(f"reading cfbfastR pbp for year {year}")
    pbp24 = pd.read_parquet(f"https://github.com/sportsdataverse/sportsdataverse-data/releases/download/cfbfastR_cfb_pbp/play_by_play_{year}.parquet")

    pbp = pbp24[
        (pbp24.home_team_division == "fbs")
        & (pbp24.away_team_division == "fbs")
        & (pbp24.home_wp_before >= 0.1)
        & (pbp24.home_wp_before <= 0.9)
        & (pbp24.EPA.notna())
    ]
    print(f"found {len(pbp)} relevant plays, adding necessary vars")

    pbp["pos_team_id"] = np.select(
        [
            pbp["pos_team"] == pbp["home"],
            pbp["pos_team"] == pbp["away"]
        ],
        [
            pbp["home_team_id"].round().astype(int).astype(str),
            pbp["away_team_id"].round().astype(int).astype(str)
        ],
        default = None
    )

    pbp["pos_team_conference"] = np.select(
        [
            pbp["pos_team"] == pbp["home"],
            pbp["pos_team"] == pbp["away"]
        ],
        [
            pbp["home_team_conference"],
            pbp["away_team_conference"]
        ],
        default = None
    )

    pbp["def_pos_team_id"] = np.select(
        [
            pbp["pos_team"] == pbp["away"],
            pbp["pos_team"] == pbp["home"]
        ],
        [
            pbp["home_team_id"].round().astype(int).astype(str),
            pbp["away_team_id"].round().astype(int).astype(str)
        ],
        default = None
    )

    pbp["def_pos_team_conference"] = np.select(
        [
            pbp["pos_team"] == pbp["away"],
            pbp["pos_team"] == pbp["home"]
        ],
        [
            pbp["home_team_conference"],
            pbp["away_team_conference"]
        ],
        default = None
    )

    pbp["hfa"] = np.select(
        [
            pbp["neutral_site"] == True,
            pbp["pos_team"] == pbp["home"]
        ],
        [
            0,
            1
        ],
        default = -1
    )

    pbp = pbp[["game_id", "pos_team_id", "pos_team", "pos_team_conference", "def_pos_team_id", "def_pos_team", "def_pos_team_conference", "EPA", "hfa"]]
    return pbp


def apply_ridge_regression(pbp: pd.DataFrame) -> pd.DataFrame:
    print(f"Creating dummy variables for {len(pbp)} plays")
    dfDummies = pd.get_dummies(pbp[["pos_team_id", "hfa", "def_pos_team_id"]])

    # Hyperparameter tuning for alpha (aka lambda, ie the penalty term)
    # for full season PBP data, the alpha will be 150-200, for smaller sample sizes it may find a higher alpha
    rdcv = linear_model.RidgeCV(alphas = [75,100,125,150,175,200,225,250,275,300,325], fit_intercept = True)
    rdcv.fit(dfDummies, pbp["EPA"]);
    alf = rdcv.alpha_

    print(f"Applying ridge with target lambda/alpha: {alf}")
    # Set up ridge regression model parameters
    reg = linear_model.Ridge(alpha = alf, fit_intercept = True)
    reg.fit(X = dfDummies, y = pbp["EPA"])

    dfRegResults = pd.DataFrame({
        'coef_name': dfDummies.columns.values,
        'ridge_reg_coef': reg.coef_}
    )

    # Add intercept back in to reg coef to get 'adjusted' value
    dfRegResults['ridge_reg_value'] = (dfRegResults['ridge_reg_coef']+reg.intercept_)

    #Print the HFA and Alpha values
    print(f"Applied ridge. base value - {reg.intercept_}")
    print('{:.3f}'.format(dfRegResults[dfRegResults['coef_name'] == "hfa"]['ridge_reg_coef'][0]))


    # Offense
    offStr = "pos_team_id"
    dfAdjOff = (dfRegResults[dfRegResults['coef_name'].str.slice(0, len(offStr)) == offStr].rename(columns = {"ridge_reg_value": "EPA"}).reset_index(drop = True))
    dfAdjOff[offStr] = dfAdjOff['coef_name'].str.replace(offStr+'_','')
    dfAdjOff = dfAdjOff.drop(columns=['ridge_reg_coef', 'coef_name'])

    # Defense
    defStr = "def_pos_team_id"
    dfAdjDef = (dfRegResults[dfRegResults['coef_name'].str.slice(0, len(defStr)) == defStr].rename(columns = {"ridge_reg_value": "EPA"}).reset_index(drop = True))
    dfAdjDef[defStr] = dfAdjDef['coef_name'].str.replace(defStr+'_','')
    dfAdjDef = dfAdjDef.drop(columns=['ridge_reg_coef', 'coef_name'])

    dfTeam = pd.merge(
        pbp[["pos_team_id", "pos_team", "pos_team_conference"]].drop_duplicates().rename({ "pos_team": "team", "pos_team_id": "team_id", "pos_team_conference": "team_conference" },axis=1),
        pbp.groupby('pos_team_id').EPA.mean().reset_index().rename({"EPA": "rawOffEPA"},axis=1),
        how="left",
        left_on="team_id",
        right_on="pos_team_id"
    ).drop(["pos_team_id"], axis=1)

    dfTeam = pd.merge(
        dfTeam,
        pbp.groupby('def_pos_team_id').EPA.mean().reset_index().rename({"EPA": "rawDefEPA"},axis=1),
        how="left",
        left_on="team_id",
        right_on="def_pos_team_id"
    ).drop(["def_pos_team_id"], axis=1)

    dfTeam = pd.merge(
        dfTeam,
        dfAdjOff.rename({"EPA": "adjOffEPA"},axis=1),
        how="left",
        left_on="team_id",
        right_on="pos_team_id"
    ).drop(["pos_team_id"], axis=1)


    dfTeam = pd.merge(
        dfTeam,
        dfAdjDef.rename({"EPA": "adjDefEPA"},axis=1),
        how="left",
        left_on="team_id",
        right_on="def_pos_team_id"
    ).drop(["def_pos_team_id"], axis=1)


    dfTeam = pd.merge(
        dfTeam,
        pbp.groupby('pos_team_id').agg({"game_id": lambda x: x.nunique() }).reset_index().rename({"game_id": "n_games"},axis=1),
        how="left",
        left_on="team_id",
        right_on="pos_team_id"
    ).drop(["pos_team_id"], axis=1)

    print(f"Collated ridge values for {len(dfTeam)} teams")


    # final formatting and output
    print(f"Adding adjusted ranks for {len(dfTeam)} teams")
    dfTeam = dfTeam.round(3) # round adjusted value to thousandths
    dfTeam["netAdjEPA"] = dfTeam["adjOffEPA"] - dfTeam["adjDefEPA"]
    dfTeam["adjOffEPA_rank"] = dfTeam.adjOffEPA.rank(method="dense", ascending=False)
    dfTeam["adjDefEPA_rank"] = dfTeam.adjDefEPA.rank(method="dense", ascending=True)
    dfTeam["netAdjEPA_rank"] = dfTeam.netAdjEPA.rank(method="dense", ascending=False)

    return dfTeam

for yr in range(2014, 2026):
    print(f"Starting ridge regression adjustments for year {yr}")
    p = retrieve_pbp(yr)
    adjEP = apply_ridge_regression(p)

    print(f"Cleaning ridge data for write...")
    adjEP.rename({
        "adjOffEPA": "adj_off_epa",
        "adjOffEPA_rank": "adj_off_epa_rank",
        "adjDefEPA": "adj_def_epa",
        "adjDefEPA_rank": "adj_def_epa_rank",
        "netAdjEPA": "net_adj_epa",
        "netAdjEPA_rank": "net_adj_epa_rank"
    },axis=1,inplace=True)

    adjEP = adjEP[["team_id", "adj_off_epa", "adj_off_epa_rank", "adj_def_epa", "adj_def_epa_rank", "net_adj_epa", "net_adj_epa_rank"]]
    adjEP["team_id"] = adjEP["team_id"].astype(int)

    print(f"Merging and writing new CSV...")
    overall_data = pd.read_csv(f"./data/{yr}/overall.csv")
    overall_data = pd.merge(
        overall_data,
        adjEP,
        on="team_id",
        how="left"
    )
    overall_data.to_csv(f"./data/{yr}/overall.csv", index=False)
    print(f"Wrote new CSV. Adjustments done for {yr}")