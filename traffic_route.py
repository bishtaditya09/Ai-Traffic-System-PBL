import streamlit as st
from geopy.geocoders import Nominatim
import folium
from streamlit_folium import folium_static
import requests
import polyline
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Set up geolocator
geolocator = Nominatim(user_agent="traffic_app")

# Title
st.title("🌍 Smart Traffic Route Planner")

# Region selection
region = st.selectbox("Choose your region", ["Delhi", "Mumbai", "Bangalore", "Hyderabad", "Kolkata"])

# Time input
col1, col2 = st.columns(2)
with col1:
    selected_hour = st.slider("Select hour", 0, 23, 9)
with col2:
    selected_day = st.selectbox("Day", ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"])

# Locations
current_loc = st.text_input("Your current location")
destination = st.text_input("Destination")

# Get congestion prediction from R API
def get_congestion_prediction(day, hour):
    try:
        res = requests.get("http://localhost:8000/predict", params={"day": day, "hour": hour})
        if res.status_code == 200:
            return float(res.json()["prediction"])
        else:
            return None
    except:
        return None

# Route drawing
if st.button("Find Best Route"):
    if current_loc and destination:
        try:
            # Get coordinates
            current_coords = geolocator.geocode(current_loc)
            dest_coords = geolocator.geocode(destination)

            if current_coords and dest_coords:
                # Map setup centered at start
                m = folium.Map(location=[current_coords.latitude, current_coords.longitude], zoom_start=12)
                folium.Marker([current_coords.latitude, current_coords.longitude],
                              tooltip="Your Location",
                              icon=folium.Icon(color="green")).add_to(m)
                folium.Marker([dest_coords.latitude, dest_coords.longitude],
                              tooltip="Destination",
                              icon=folium.Icon(color="red")).add_to(m)

                # Function to get route from OSRM
                def get_road_route(start_lat, start_lon, end_lat, end_lon):
                    
                    import requests, polyline
                    try:
                
                      url = f"http://router.project-osrm.org/route/v1/driving/{start_lon},{start_lat};{end_lon},{end_lat}?overview=full&geometries=polyline"
                      response = requests.get(url, timeout=10)
                      data = response.json()
                      if data.get('code') == 'Ok' and len(data['routes']) > 0:
                            geometry = data['routes'][0]['geometry']
                            decoded = polyline.decode(geometry)
                            duration = data['routes'][0]['duration'] / 60  # seconds to minutes
                            return decoded, duration
                      else:
                            st.warning("⚠ Failed to get road-based route. Falling back to straight line.")
                            return None, None
                    except Exception as e:
                        st.error(f"🚫 OSRM routing error: {e}")
                        return None, None

                # Get route points and base travel time
                route_points, base_duration = get_road_route(current_coords.latitude, current_coords.longitude,
                                                             dest_coords.latitude, dest_coords.longitude)

                # Show route polyline on map
                if route_points:
                    folium.PolyLine(
                        locations=route_points,
                        color="#1E90FF", weight=6, opacity=0.8
                    ).add_to(m)
                else:
                    folium.PolyLine(
                        locations=[[current_coords.latitude, current_coords.longitude],
                                   [dest_coords.latitude, dest_coords.longitude]],
                        color="blue", weight=5, dash_array="5, 5"
                    ).add_to(m)

                # Show map
                st.subheader("🗺 Best Route (via Road)")
                folium_static(m)

                # Call R prediction API for congestion
                try:
                    api_url = "http://localhost:8000/predict"
                    params = {"day": selected_day, "hour": selected_hour, "region": region}
                    resp = requests.get(api_url, params=params, timeout=10)
                    if resp.status_code == 200:
                        prediction_data = resp.json()
                        congestion_pct = float(prediction_data.get("prediction", 0))
                    else:
                        st.warning("⚠ Prediction API error, using default congestion 0%.")
                        congestion_pct = 0
                except Exception as e:
                    st.warning(f"⚠ Could not connect to prediction API: {e}")
                    congestion_pct = 0

                # Adjust travel time by congestion %
                if base_duration is not None:
                    adjusted_duration = base_duration * (1 + congestion_pct / 100)
                    st.markdown(f"🕓 *Estimated travel time:* {int(adjusted_duration)} minutes")
                else:
                    st.markdown("🕓 Estimated travel time: Unknown")

                # Show congestion info nicely
                st.subheader("🚦 Predicted Traffic Conditions")
                def region_congestion(region, hour):
                    if region == "Delhi":
                        if 8 <= hour <= 11 or 17 <= hour <= 20:
                            return "High (85%)", "red", "Peak city traffic"
                        elif 11 <= hour <= 16:
                            return "Medium (50%)", "orange", "Regular congestion"
                        else:
                            return "Low (25%)", "green", "Off-peak hours"
                    elif region == "Bangalore":
                        if 7 <= hour <= 10 or 17 <= hour <= 21:
                            return "High (90%)", "red", "Tech hub rush"
                        else:
                            return "Medium (60%)", "orange", "Urban flow"
                    elif region == "Mumbai":
                        if 8 <= hour <= 11 or 18 <= hour <= 21:
                            return "High (80%)", "red", "Business area congestion"
                        else:
                            return "Low (30%)", "green", "Smooth movement"
                    else:
                        if 9 <= hour <= 11 or 17 <= hour <= 19:
                            return "Medium (50%)", "orange", "Moderate flow"
                        else:
                            return "Low (20%)", "green", "Light city traffic"

                congestion_level, color, reason = region_congestion(region, selected_hour)

                st.markdown(
                    f'<div style="background-color:{color};padding:10px;border-radius:5px;color:white;margin-bottom:20px;">'
                    f'<b>City:</b> {region}<br>'
                    f'<b>Time:</b> {selected_hour}:00 on {selected_day}<br>'
                    f'<b>Congestion:</b> {congestion_level} (Predicted: {congestion_pct:.1f}%)<br>'
                    f'<b>Reason:</b> {reason}<br>'
                    f'</div>',
                    unsafe_allow_html=True
                )

                st.subheader("ℹ Route Details")
                st.info("""
                1. Real road path fetched from OSRM API
                2. Predicted traffic based on local rush hours and ML model from R API
                3. Estimated travel time adjusted with predicted congestion
                """)

            else:
                st.error("Could not geolocate your input. Please recheck.")
        except Exception as e:
            st.error(f"Error: {str(e)}")
    else:
        st.warning("Please enter both current location and destination")

