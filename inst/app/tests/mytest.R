app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$snapshot()
app$setInputs(level1 = "Akuttinnleggelser")
app$setInputs(level2 = "Ventilasjonsstøtte (andeler)")
app$snapshot()
app$snapshot()
# Input 'leafletmap_shape_mouseover' was set, but doesn't have an input binding.
# Input 'leafletmap_shape_mouseout' was set, but doesn't have an input binding.
# Input 'leafletmap_shape_mouseover' was set, but doesn't have an input binding.
# Input 'leafletmap_shape_mouseout' was set, but doesn't have an input binding.
app$snapshot()
