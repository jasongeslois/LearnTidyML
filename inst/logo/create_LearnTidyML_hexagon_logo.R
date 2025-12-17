# LearnTidyML Hex Logo Creation Script - Workflow Diagram Version
# Features: Thick slate-blue path flowing from bottom-left through central compass node
# branching into 4 curved arrows pointing upward-right with placeholder icons
# Style matching KnowledgeR and PassR logos

# Install required packages if needed
required_pkgs <- c("ggplot2", "showtext", "sysfonts", "png", "grid", "patchwork")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(ggplot2)
library(showtext)
library(sysfonts)
library(png)
library(grid)
library(patchwork)

# Add a nice font
font_add_google("Roboto Condensed", "roboto")
showtext_auto()

# Create output directory if it doesn't exist
# Use the directory where this script is located
output_dir <- dirname(sys.frame(1)$ofile)
if (is.null(output_dir) || output_dir == "") {
  # Fallback: assume script is run from package root
  output_dir <- "inst/logo"
}
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Function to create hexagon vertices
hex_vertices <- function(cx = 0, cy = 0, size = 1) {
  angles <- seq(pi/6, 2*pi + pi/6, length.out = 7)  # Start at 30 degrees for flat-top hex
  data.frame(
    x = cx + size * cos(angles),
    y = cy + size * sin(angles)
  )
}

# ============================================================================
# COMPASS PLOT PLACEHOLDER
# PASTE EXISTING COMPASS R CODE HERE
# This creates a smaller compass to be placed at the central branching node
# ============================================================================
create_compass_plot <- function() {
  # Compass design - scaled smaller for central node
  scale <- 0.22  # Much smaller to fit as central node
  cy <- 0.05     # Slight vertical offset for centering in workflow

  # Outer compass circle
  circle_outer <- data.frame(
    x = cos(seq(0, 2*pi, length.out = 100)) * 0.9 * scale,
    y = sin(seq(0, 2*pi, length.out = 100)) * 0.9 * scale + cy
  )

  # Inner circle (center hub)
  circle_inner <- data.frame(
    x = cos(seq(0, 2*pi, length.out = 100)) * 0.12 * scale,
    y = sin(seq(0, 2*pi, length.out = 100)) * 0.12 * scale + cy
  )

  # Tick marks - 12 positions
  tick_angles <- seq(0, 2*pi, length.out = 13)[-13]
  ticks_outer <- data.frame(
    x = cos(tick_angles) * 0.85 * scale,
    y = sin(tick_angles) * 0.85 * scale + cy
  )
  ticks_inner <- data.frame(
    x = cos(tick_angles) * 0.72 * scale,
    y = sin(tick_angles) * 0.72 * scale + cy
  )

  # Cardinal ticks (N, E, S, W) - longer
  cardinal_angles <- c(pi/2, 0, -pi/2, pi)
  cardinal_outer <- data.frame(
    x = cos(cardinal_angles) * 0.88 * scale,
    y = sin(cardinal_angles) * 0.88 * scale + cy
  )
  cardinal_inner <- data.frame(
    x = cos(cardinal_angles) * 0.62 * scale,
    y = sin(cardinal_angles) * 0.62 * scale + cy
  )

  # Compass needle pointing upper-right (optimal direction)
  needle_angle <- pi/4

  # Main needle (north/recommended direction) - cyan/teal
  needle_n <- data.frame(
    x = c(0, 0.06 * scale, cos(needle_angle) * 0.55 * scale, -0.06 * scale, 0),
    y = c(cy, cy + 0.06 * scale, sin(needle_angle) * 0.55 * scale + cy, cy + 0.06 * scale, cy)
  )

  # Opposite needle (south) - darker blue
  needle_s <- data.frame(
    x = c(0, 0.06 * scale, cos(needle_angle + pi) * 0.55 * scale, -0.06 * scale, 0),
    y = c(cy, cy - 0.06 * scale, sin(needle_angle + pi) * 0.55 * scale + cy, cy - 0.06 * scale, cy)
  )

  # ML method nodes around the compass
  node_angles <- c(pi/6, pi/3, 2*pi/3, 5*pi/6, 7*pi/6, 4*pi/3, 5*pi/3, 11*pi/6)
  nodes <- data.frame(
    x = cos(node_angles) * 0.5 * scale,
    y = sin(node_angles) * 0.5 * scale + cy
  )

  # Return compass components as a list
  list(
    circle_outer = circle_outer,
    circle_inner = circle_inner,
    ticks_outer = ticks_outer,
    ticks_inner = ticks_inner,
    cardinal_outer = cardinal_outer,
    cardinal_inner = cardinal_inner,
    needle_n = needle_n,
    needle_s = needle_s,
    nodes = nodes,
    cy = cy,
    scale = scale
  )
}

# Function to create smooth curved arrow path
create_curved_arrow <- function(start_x, start_y, end_x, end_y, curvature = 0.3, n_points = 50) {
  # Create a bezier-like curve
  t <- seq(0, 1, length.out = n_points)

  # Control point for curve
  mid_x <- (start_x + end_x) / 2
  mid_y <- (start_y + end_y) / 2

  # Perpendicular offset for curvature
  dx <- end_x - start_x
  dy <- end_y - start_y
  length <- sqrt(dx^2 + dy^2)

  # Perpendicular vector
  perp_x <- -dy / length * curvature
  perp_y <- dx / length * curvature

  ctrl_x <- mid_x + perp_x
  ctrl_y <- mid_y + perp_y

  # Quadratic bezier curve
  x <- (1-t)^2 * start_x + 2*(1-t)*t * ctrl_x + t^2 * end_x
  y <- (1-t)^2 * start_y + 2*(1-t)*t * ctrl_y + t^2 * end_y

  data.frame(x = x, y = y)
}

# Function to create arrowhead
create_arrowhead <- function(tip_x, tip_y, angle, size = 0.08) {
  # Arrowhead pointing in direction of angle
  left_angle <- angle + pi + pi/6
  right_angle <- angle + pi - pi/6

  data.frame(
    x = c(tip_x, tip_x + size * cos(left_angle), tip_x + size * cos(right_angle), tip_x),
    y = c(tip_y, tip_y + size * sin(left_angle), tip_y + size * sin(right_angle), tip_y)
  )
}

# Create the full workflow logo
create_LearnTidyML_workflow_logo <- function() {

  # Hexagon parameters
  outer_hex <- hex_vertices(0, 0, 1)
  inner_hex <- hex_vertices(0, 0, 0.92)

  # ========================================
  # WORKFLOW PATH DESIGN
  # ========================================

  # Central node position (where compass will be placed)
  center_x <- -0.05
  center_y <- 0.05

  # Muted slate blue color for paths
  path_color <- "#607D8B"  # Slate blue
  path_color_light <- "#78909C"  # Lighter slate

  # ==========================================================================
  # ADJUSTMENT GUIDE FOR FUTURE EDITS:
  #
  # To move workflow elements INSIDE hexagon: reduce end_x/end_y values below
  # To move workflow elements OUTSIDE hexagon: increase end_x/end_y values
  # Inner hexagon edge is at ~0.80 (x) depending on y position
  #
  # Key parameters:
  #   - input_path start_x/start_y: where the input arrow begins
  #   - branch end_x/end_y: where each output arrow ends (arrow tip position)
  #   - curvature: how curved each path is (higher = more curved)
  # ==========================================================================

  # Input path: from bottom-left to center
  input_path <- create_curved_arrow(
    start_x = -0.55, start_y = -0.45,
    end_x = center_x - 0.15, end_y = center_y - 0.05,
    curvature = 0.12, n_points = 60
  )

  # Four output branches curving upward-right
  # Branch 1: Top-most (end_x, end_y control arrow tip position)
  branch1 <- create_curved_arrow(
    start_x = center_x + 0.15, start_y = center_y + 0.05,
    end_x = 0.35, end_y = 0.49,
    curvature = 0.18, n_points = 50
  )

  # Branch 2: Upper-middle
  branch2 <- create_curved_arrow(
    start_x = center_x + 0.15, start_y = center_y,
    end_x = 0.52, end_y = 0.32,
    curvature = 0.12, n_points = 50
  )

  # Branch 3: Lower-middle
  branch3 <- create_curved_arrow(
    start_x = center_x + 0.15, start_y = center_y - 0.05,
    end_x = 0.55, end_y = 0.10,
    curvature = 0.08, n_points = 50
  )

  # Branch 4: Bottom-most
  branch4 <- create_curved_arrow(
    start_x = center_x + 0.12, start_y = center_y - 0.10,
    end_x = 0.50, end_y = -0.12,
    curvature = 0.04, n_points = 50
  )

  # Calculate arrowhead angles for each branch
  get_end_angle <- function(path) {
    n <- nrow(path)
    dx <- path$x[n] - path$x[n-3]
    dy <- path$y[n] - path$y[n-3]
    atan2(dy, dx)
  }

  # ==========================================================================
  # ARROWHEAD ADJUSTMENT GUIDE:
  #   - First two args (tip_x, tip_y): must match branch end_x/end_y above
  #   - size: controls arrowhead size (larger = bigger triangle)
  # ==========================================================================
  arrow1 <- create_arrowhead(0.40, 0.51, get_end_angle(branch1), size = 0.06)
  arrow2 <- create_arrowhead(0.56, 0.33, get_end_angle(branch2), size = 0.06)
  arrow3 <- create_arrowhead(0.59, 0.10, get_end_angle(branch3), size = 0.06)
  arrow4 <- create_arrowhead(0.53, -0.13, get_end_angle(branch4), size = 0.06)

  # ========================================
  # PLACEHOLDER ICONS - positioned BEYOND arrowheads
  # Arrows point TO the icons (both visible, no overlap)
  # ========================================
  #
  # ==========================================================================
  # ICON ADJUSTMENT GUIDE:
  #   - Icons are offset ~0.08-0.10 beyond the arrow tips
  #   - icon_offset: distance from arrow tip to icon center
  #   - To move icons closer to arrows: decrease the offset values
  #   - To move icons further from arrows: increase the offset values
  # ==========================================================================

  # Offset distance from arrow tip to icon center
  icon_offset <- 0.10

  # Icon 1: Bar chart - positioned beyond arrow1 (pointing up-right)
  # Arrow1 ends at (0.35, 0.49), icon placed further along that direction
  bar_icon <- data.frame(
    xmin = c(0.42, 0.46, 0.50),
    xmax = c(0.45, 0.49, 0.53),
    ymin = c(0.51, 0.48, 0.53),
    ymax = c(0.58, 0.58, 0.58)
  )

  # Icon 2: Line chart - positioned beyond arrow2
  # Arrow2 ends at (0.52, 0.32), icon placed further right/up
  line_icon <- data.frame(
    x = c(0.58, 0.62, 0.66, 0.70),
    y = c(0.32, 0.38, 0.33, 0.39)
  )

  # Icon 3: Scatter plot - positioned beyond arrow3
  # Arrow3 ends at (0.55, 0.10), icon placed further right
  scatter_icon <- data.frame(
    x = c(0.62, 0.65, 0.63, 0.67, 0.64),
    y = c(0.09, 0.13, 0.16, 0.11, 0.05)
  )

  # Icon 4: Pie chart - positioned beyond arrow4
  # Arrow4 ends at (0.50, -0.12), icon placed further right/down
  pie_angles <- seq(0, 2*pi, length.out = 50)
  pie_icon <- data.frame(
    x = 0.59 + 0.05 * cos(pie_angles),
    y = -0.15 + 0.05 * sin(pie_angles)
  )

  # Get compass components
  compass <- create_compass_plot()

  # ========================================
  # BUILD THE PLOT
  # ========================================
  p <- ggplot() +
    # Outer hexagon (border)
    geom_polygon(data = outer_hex, aes(x = x, y = y),
                 fill = "#37474F", color = "#37474F", linewidth = 0) +

    # Inner hexagon (main background) - white as requested
    geom_polygon(data = inner_hex, aes(x = x, y = y),
                 fill = "#FFFFFF", color = "#FFFFFF", linewidth = 0) +

    # ========================================
    # WORKFLOW PATHS - Thick slate-blue curves
    # ========================================

    # Input path (thickest, main flow)
    geom_path(data = input_path, aes(x = x, y = y),
              color = path_color, linewidth = 4, lineend = "round") +

    # Branch paths
    geom_path(data = branch1, aes(x = x, y = y),
              color = path_color, linewidth = 3, lineend = "round") +
    geom_path(data = branch2, aes(x = x, y = y),
              color = path_color, linewidth = 3, lineend = "round") +
    geom_path(data = branch3, aes(x = x, y = y),
              color = path_color, linewidth = 3, lineend = "round") +
    geom_path(data = branch4, aes(x = x, y = y),
              color = path_color, linewidth = 3, lineend = "round") +

    # Arrowheads
    geom_polygon(data = arrow1, aes(x = x, y = y), fill = path_color, color = path_color) +
    geom_polygon(data = arrow2, aes(x = x, y = y), fill = path_color, color = path_color) +
    geom_polygon(data = arrow3, aes(x = x, y = y), fill = path_color, color = path_color) +
    geom_polygon(data = arrow4, aes(x = x, y = y), fill = path_color, color = path_color) +

    # ========================================
    # PLACEHOLDER ICONS (charts/data outputs)
    # ========================================

    # Bar chart icon
    geom_rect(data = bar_icon, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "#455A64", color = NA) +

    # Line chart icon
    geom_path(data = line_icon, aes(x = x, y = y),
              color = "#455A64", linewidth = 1.5) +

    # Scatter plot icon
    geom_point(data = scatter_icon, aes(x = x, y = y),
               color = "#455A64", size = 2) +

    # Pie chart icon
    geom_polygon(data = pie_icon, aes(x = x, y = y),
                 fill = "#455A64", color = "#37474F", linewidth = 0.5) +
    # Pie slice lines (center at 0.58, -0.16 to match pie_icon)
    geom_segment(aes(x = 0.58, y = -0.16, xend = 0.58 + 0.05, yend = -0.16),
                 color = "#FFFFFF", linewidth = 1) +
    geom_segment(aes(x = 0.58, y = -0.16, xend = 0.58, yend = -0.16 + 0.05),
                 color = "#FFFFFF", linewidth = 1) +

    # ========================================
    # CENTRAL COMPASS NODE
    # ========================================

    # Compass background circle (slightly larger for visual separation)
    geom_polygon(
      data = data.frame(
        x = center_x + cos(seq(0, 2*pi, length.out = 100)) * 0.19,
        y = center_y + sin(seq(0, 2*pi, length.out = 100)) * 0.19
      ),
      aes(x = x, y = y),
      fill = "#1A237E", color = "#37474F", linewidth = 1
    ) +

    # Outer compass ring
    geom_path(
      data = data.frame(
        x = center_x + compass$circle_outer$x,
        y = center_y + compass$circle_outer$y - compass$cy
      ),
      aes(x = x, y = y),
      color = "#B0BEC5", linewidth = 1
    ) +

    # Regular tick marks
    geom_segment(data = data.frame(
      x = center_x + compass$ticks_inner$x,
      y = center_y + compass$ticks_inner$y - compass$cy,
      xend = center_x + compass$ticks_outer$x,
      yend = center_y + compass$ticks_outer$y - compass$cy
    ), aes(x = x, y = y, xend = xend, yend = yend),
    color = "#78909C", linewidth = 0.4) +

    # Cardinal tick marks (longer, brighter)
    geom_segment(data = data.frame(
      x = center_x + compass$cardinal_inner$x,
      y = center_y + compass$cardinal_inner$y - compass$cy,
      xend = center_x + compass$cardinal_outer$x,
      yend = center_y + compass$cardinal_outer$y - compass$cy
    ), aes(x = x, y = y, xend = xend, yend = yend),
    color = "#ECEFF1", linewidth = 0.8) +

    # Compass needle - south part (darker orange)
    geom_polygon(
      data = data.frame(
        x = center_x + compass$needle_s$x,
        y = center_y + compass$needle_s$y - compass$cy
      ),
      aes(x = x, y = y),
      fill = "#FF7043", color = "#F4511E", linewidth = 0.2
    ) +

    # Compass needle - north part (bright orange)
    geom_polygon(
      data = data.frame(
        x = center_x + compass$needle_n$x,
        y = center_y + compass$needle_n$y - compass$cy
      ),
      aes(x = x, y = y),
      fill = "#FFA726", color = "#FB8C00", linewidth = 0.2
    ) +

    # Center hub
    geom_polygon(
      data = data.frame(
        x = center_x + compass$circle_inner$x,
        y = center_y + compass$circle_inner$y - compass$cy
      ),
      aes(x = x, y = y),
      fill = "#263238", color = "#B0BEC5", linewidth = 0.5
    ) +

    # Center dot
    geom_point(aes(x = center_x, y = center_y), color = "#ECEFF1", size = 1) +

    # ========================================
    # PACKAGE NAME
    # ========================================
    annotate("text", x = 0.38, y = -0.59, label = "LearnTidyML",
             color = "#37474F", size = 30, fontface = "bold",
             family = "roboto", angle = 30) +

    # Theme
    coord_fixed(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(0, 0, 0, 0)
    )

  return(p)
}

# Generate the logo
logo_plot <- create_LearnTidyML_workflow_logo()

# Save as PNG (with "2" added to filename as requested)
ggsave(
  filename = file.path(output_dir, "LearnTidyML_logo.png"),
  plot = logo_plot,
  width = 5.5,
  height = 5.5,
  dpi = 300,
  bg = "transparent"
)

cat("Workflow logo created successfully!\n")
cat("Output:", file.path(output_dir, "LearnTidyML_logo.png"), "\n")
