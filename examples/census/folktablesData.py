# Python commands to get data in caData.Rds

from folktables import ACSDataSource, ACSIncome

data_source = ACSDataSource(survey_year = '2015', horizon = '1-Year', survey = 'person')
acs_data = data_source.get_data(states = ["CA"], download=True)

ca_data = acs_data[["AGEP", "COW", "SCHL", "MAR", "OCCP", "POBP", 
                    "RELP", "WKHP", "SEX", "RAC1P", "PINCP", "PWGTP"]]

