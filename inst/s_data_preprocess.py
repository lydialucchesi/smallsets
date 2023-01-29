# smallsets start s_data caption[Remove rows where C2
# is FALSE.]caption
s_data = s_data[s_data["C2"] == True]

s_data["C6"].fillna(value = s_data["C6"].mean(), inplace = True)
# smallsets snap s_data caption[Replace missing values in C6 and
# C8 with column means. Drop C7 because there are too many
# missing values.]caption
s_data["C8"].fillna(value = s_data["C8"].mean(), inplace = True)
s_data = s_data.drop(columns = ["C7"])

s_data["C9"] = s_data["C3"] + s_data["C4"]
# smallsets end s_data caption[Create a new column,
# C9, by summing C3 and C4.]caption
