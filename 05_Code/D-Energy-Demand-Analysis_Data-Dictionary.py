#-*- coding: utf-8 -*-

"""
Description
: To generate
    1) Dictionaries for renaming columns of DFs
    2) A dictionary containing labels of columns
"""


# --------------------------------------------------
# To import module(s) and package(s) required
# --------------------------------------------------
# (NOT Applicable)


# --------------------------------------------------
# To make dictionaries for renaming columns of DFs
# --------------------------------------------------
# ------- To make dictionaries for SMUD Data -------
# # 1. Billing Data
dict_toRename_billing = {
    'cont_acc_id': 'id_account',
    'premise_id': 'id_premise',
    'rate_cat': 'rate_code',
    'dat_from': 'period_from',
    'dat_to': 'period_to',
    'cust_chg': 'charge_fixed',
    'kwh': 'kwh_total',
    'kwh_chg': 'charge_variable',
    't1kwh': 'kwh_t1',
    't1kwh_ch': 'charge_variable_t1',
    't2kwh': 'kwh_t2',
    't2kwh_ch': 'charge_variable_t2',
    't3kwh': 'kwh_t3',
    't3kwh_ch': 'charge_variable_t3',
    'tot_bill': 'charge_total', 
    'pv': 'is_pv',
    'bu_part_id': 'id_bu_part'
}

# # 2. ID Data
dict_toRename_id = {
    'cont_acc_id': 'id_account',
    'bu_part_id': 'id_bu_part',
    'premise_id': 'id_premise',
    'isa': 'is_in_isa',
    'rate_cat_demog': 'rate_code_from_id',
    'move_in_date': 'date_move_in',
    'move_out_date': 'date_move_out',
    'house_type': 'type_house',
    'scity': 'city',
    'szip': 'zip'
}


# --------------------------------------------------
# To make a dictionary containing labels of columns
# --------------------------------------------------
# ------- To make a dictionary containing labels of columns -------
dict_labels = {
    'charge_fixed_usd': 'Amount of Fixed Charge',
    'charge_in_usd': 'Charge in USD',
    'charge_total': 'Amount of Total Charge',
    'charge_variable': 'Amount of Variable Charge',
    'charge_variable_t1': 'Amount of Variable Charge that is subject to Tier 1',
    'charge_variable_t2': 'Amount of Variable Charge that is subject to Tier 2',
    'charge_variable_t3': 'Amount of Variable Charge that is subject to Tier 3',
    'city': 'City Name',
    'date_move_in': 'Move-In Date',
    'date_move_out': 'Move-Out Date',
    'id_account': 'Account ID',
    'id_bu_part': 'ID for given Account ID, Premise ID, and Rate Code',
    'id_premise': 'Premise ID',
    'is_in_isa': 'Indicating whether a bill was in Interim Service Agreement (ISA)',
    'is_pv': 'Indicating whether a premise was on Solar rate',
    'kwh_total': 'Total kWh consumed',
    'kwh_t1': 'kWh that are subject to Tier 1',
    'kwh_t2': 'kWh that are subject to Tier 2',
    'kwh_t3': 'kWh that are subject to Tier 3',
    'period_from': 'Begin Date of a Billing Period',
    'period_len': 'Length of a Billing Period',
    'period_to': 'End Date of a Billing Period',
    'rate_code': 'Rate Code in Billing Data',
    'rate_code_from_id' : 'Rate Code in ID Data',
    'type_house': 'House Type',
    'revision_date': 'Date at which SMUD Residential Rate Schedules revised',
    'revision_year': 'Year at which SMUD Residential Rate Schedules revised',
    'zip': 'ZIP Code',
}
