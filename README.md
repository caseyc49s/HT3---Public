# Honda Team 3 Repository

This repository is for collaboration between team members on the final project for the Spring 2019 Data-X at UC Berkeley.

## __Our Project__ (_please edit as this changes_)

__TOPIC__: How early should an ADAS warning be presented to be most effective.

  - __Part1__: Compare the driver behavior at smart vs non smart intersections (metrics: acceleration, braking, speed, location, flow, number of messages received etc)  
    + Visualize/ Classify Intersections
        + Find Intersection Coordinates
        + Split into smart vs. non-smart
        + Can look at SPAT table for non-smart intersection IDs
    + Determine which intersection Host vehicle is heading to
        + Use location and orientation (in Host Table)
        + Can also use Google Maps
    + Aggregate Stats for smart vs. non-smart
    + Add Findings in UI

  - __Part2__: Determine optimal location of future smart intersections based on intersection characteristics
    + Safety
        + Possibly Merge Collision Data
        + Weather
        + Event Flags
    + Find most similar Intersections
        + Number of lanes
        + Distance from one to the next
    + Add to UI (make interactive map GUI)

  - __UI__: Could be a Interactive Map of Findings

<https://transportationops.org/sites/transops/files/SPaT%20challenge%20Folio%20imposed.pdf>
