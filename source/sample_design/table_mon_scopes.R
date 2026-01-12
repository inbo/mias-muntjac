table_mon_scopes <- data.frame(
  index = c("A", "B", "C", "D"),
  name = c(
    "confirm population decline under management",
    "confirm eradication under management",
    "detect spread under management",
    "detect presence after management"
  ),
  description = c(
    "determine decline in population size (or presence) in relation to sustained eradication measures in distribution area",
    "determine population size (or presence) under sustained eradication measures in distribution area to confirm eradication at some point in time",
    "detect animals potentially spreading into buffer area around distribution area in relation to sustained eradication measures in distribution area",
    "detect potential left-over of re-introduced animals in (and possibly around) distribution area after eradication has been completed"
  ),
  sample_frame = c(
    "distribution area; subset of potential habitat areas",
    "distribution area; subset of potential habitat areas",
    "buffer area around distribution area; subset of potential habitat areas",
    "distribution area and potentially buffer area; subset of potential habitat areas"
    ),
  outcome = c(
    "abundance (or presence); trend over time",
    "abundance (or presence); status",
    "presence; status",
    "presence; status"
  ),
  reference_value = c(
    "zero change over time; detect (negative) deviation, hence (negative) trend",
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

