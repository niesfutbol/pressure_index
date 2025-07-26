describe("obtain_files_names", {
  it("tests/data", {
    expected <- c("Barcelona", "Bayer_Leverkusen", "Eintracht_Frankfurt", "Liverpool", "tijuana")
    obtained <- obtain_files_names("/workdir/tests/data")
    expect_equal(obtained, expected)
  })
  it("return just the team name", {
    files_name_list <- c("Barcelona.csv", "Bayer_Leverkusen.csv", "Eintracht_Frankfurt.csv", "Liverpool.csv", "tijuana.csv")
    expected <- c("Barcelona", "Bayer_Leverkusen", "Eintracht_Frankfurt", "Liverpool", "tijuana")
    obtained <- .clean_files_name(files_name_list)
    expect_equal(obtained, expected)
  })
})
