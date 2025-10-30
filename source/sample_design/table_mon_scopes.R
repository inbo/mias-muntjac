table_mon_scopes <- data.frame(
  index = c("A", "B", "C", "D"),
  name = c(
    "confirm population decline under management",
    "confirm eradication under management",
    "detect spread out of distribution area under management",
    "detect species after management"
  ),
  description = c(
    "determine decline in population size (or presence) in relation to sustained eradication measures in distribution area",
    "determine population size (or presence) under sustained eradication measures in distribution area to confirm eradication at some point in time",
    "detect animals potentially spreading into buffer area around distribution area in relation to sustained eradication measures in distribution area",
    "detect potential left-over of re-introduced animals in (and possibly around) distribution area after eradication has been completed"
  ),
  sample_frame = c(
    "distribution area",
    "distribution area",
    "buffer area around distribution area",
    "distribution area and potentially buffer area"
    ),
  outcome = c(
    "abundance (or presence); trend over time",
    "abundance (or presence); status",
    "presence; status",
    "presence; status"
  ),
  reference_value = c(
    "zero change over time; detect (negative) deviation",
    "zero abundance or presence; confirm",
    "zero presence; detect deviation",
    "zero presence; detect deviation"
  ),
  primary_sample_size_criterion = c(
    "power",
    "width of confidence / credible interval",
    "power",
    "power"
  )
)

