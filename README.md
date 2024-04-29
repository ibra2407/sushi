# Sushi Game Restaurant

## Overview
This project is a sushi restaurant simulation game implemented as a RShiny app with additional JavaScript components. The simulation aims to replicate a realistic queue management system commonly found in sushi restaurants, incorporating service operations concepts such as optimal restocking.

## Features
- **Queue Management:** Players experience a simulated queue at a sushi restaurant, where they must manage incoming orders efficiently.
- **Optimal Restocking:** The game dynamically adjusts the availability of ingredients based on demand and player actions, simulating real-world restaurant restocking strategies.
- **Performance Gauge:** At the end of the game, players receive a performance assessment, evaluating their efficiency and effectiveness in managing the restaurant.

## Technologies Used
- **RShiny:** Used to develop the main interface and backend logic of the game.
- **JavaScript:** Additional JavaScript components are integrated to enhance the user experience by storing user data in a separate SQL database.

## Installation
To run the Sushi Game Restaurant locally, follow these steps:
1. Install R, RStudio and RShiny.
2. Download the files as a zip.
3. Open the project directory in your RStudio terminal.
4. Run the RShiny app by clicking the "Run App" button at the top right hand corner of the editor screen.
5. Enjoy the game! SInce you're running it locally, you don't need to go through the login/registering segment. However your score will not tracked.

## Screenshots
Login Page
![Screenshot (672)](https://github.com/ibra2407/sushi/assets/113652688/7a8d18a3-5ecd-4bd4-b698-6672e11ec6df)
Gameplay UI - select customer group and seating lane to seat the customer group. Game simulates 7 days of a week (7 phases) and 8 hours each day (8 turns). The demand distribution closely follows real-life, with lunch and dinner peak hour demands.
![Screenshot (674)](https://github.com/ibra2407/sushi/assets/113652688/d8b35787-eb28-4022-8243-626dccdb7933)
Processing customer orders in a turn (/hour) after customers have been seated for that turn.
![Screenshot (677)](https://github.com/ibra2407/sushi/assets/113652688/40dc3717-c6e9-4ae0-98d2-07a63bdcdba9)
Not enough seats in a Seating Lane shows an alert.
![Screenshot (673)](https://github.com/ibra2407/sushi/assets/113652688/6e2ecb6a-d691-4b3d-a9ca-fdb604e3265e)
After a day has ended, player has to decide the optimal restocking order based on the day's demand for each sushi type sold.
![Screenshot (675)](https://github.com/ibra2407/sushi/assets/113652688/37c641c8-91f3-4c34-8fc8-9883972e7ee8)
Uh-oh! If player runs out of an ingredient and cannot fulfill orders, the restaurant has to shut down for that day. Game proceeds to the next day.
![Screenshot (678)](https://github.com/ibra2407/sushi/assets/113652688/119e2b49-3a0b-4ba9-a9d5-2bdf660dc4cb)
At the end of the game, player can assess their performance through the graph set out above, and understand the impact of their decisions as well as deduce the underlying demand patterns.
![Screenshot (679)](https://github.com/ibra2407/sushi/assets/113652688/d3441657-8355-4783-bc36-0e6aafc4c7d9)


