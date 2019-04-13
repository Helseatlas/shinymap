# nolint start
app <- ShinyDriver$new("../")
app$snapshotInit("testshiny")

app$setInputs()
app$snapshot()
app$setInputs(menu_level2 = "Spesialisthelsetjenesten")
app$snapshot()
app$setInputs(menu_level3 = "Poliklinikk/dag")
app$snapshot()
app$setInputs(menu_level1 = "Medisinske tilstander, spesialisthelsetjenesten")
app$snapshot()
app$setInputs(menu_level1 = "Kirurgiske tilstander, spesialisthelsetjenesten")
app$setInputs(menu_level2 = "Utvalgte prosedyrer")
app$setInputs(menu_level3 = "Fjerning av mandler")
app$snapshot()
# nolint end
