
// @ts-ignore
"use strict";

/**
 * @typedef {object} City
 * @property {string} city
 * @property {number} latitude
 * @property {number} longitude
 * @property {number} timezone
 * @property {string} timeZoneID
 */

/**
 * @param {number} offset
 * @returns {string}
 */
// create Date object for current location
function calcTime(offset) {
    let d = new Date();
    // convert to msec
    // subtract local time zone offset
    // get UTC time in msec
    let utc = d.getTime() + (d.getTimezoneOffset() * 60000);
    // create new Date object for different city
    // using supplied offset
    let nd = new Date(utc + (3600000 * offset));
    let astr = (' ' + nd).slice(5, 25) + ' UTC ';
    if (offset >= 0) {
        astr += ' + ' + offset;
    }
    else {
        astr += offset + ' ';
    }
    ;
    // return time as a string
    return astr;
}
function getOption() {
    const selectElement = document.querySelector("#select1");
    const output =
        selectElement.options[selectElement.selectedIndex].value;
    //console.log('out:', output);
    let row = output - 1;
    let name = cities[row].city;
    let lat = cities[row].latitude;
    let lon = cities[row].longitude;
    let tz = cities[row].timezone;
    //   const HTMLSelectElement =
    let local_time = new Date();
    let local_time_string = local_time.toString();
    let text = "UTC date and time  " + local_time.toUTCString().slice(5, 26);
    let dts = true; // Daylight saving
    let otherCity = cities[row].city;
    if (otherCity == 'Tokyo') {
        dts = false;
    }
    ; // No DTS in Japan!
    let otherOffset = cities[row].timezone;
    if ((dts) && (cities[row].latitude > 0)) {
        otherOffset += 1;
    }
    ; // Add DTS hour to timezone offset in northern world
    if ((!dts) && (cities[row].latitude < 0)) {
        otherOffset += 1;
    }
    ; // Add DTS hour to timezone offset in southern world
    let otherTime = calcTime(otherOffset);
    text += "<br>" + otherCity + " local time: " + otherTime + 'h';
    text += "<br>Your local time now: " + local_time_string.slice(3, 34);
    // Get current date and time 
    const d = local_time;
    const utc_hour = d.getUTCHours();
    const utc_minute = d.getUTCMinutes();
    const utc_second = d.getSeconds();
    const day = d.getUTCDate();
    const month = 1 + d.getUTCMonth();
    const full_Year = d.getFullYear();
    function dn(a, b) {
        let v = Math.floor(Math.abs(a / b));
        if (a < 0)
            v = -v;
        return v;
    }
    function toJulianDate(y, m, d) {
        const dnm = dn(m - 14, 12);
        const A = y + 4800 + dnm;
        const B = m - 2 - 12 * dnm;
        const C = dn(y + 4900 + dnm, 100);
        const JDN = dn(1461 * A, 4) + dn(367 * B, 12) - dn(3 * C, 4) + d - 32075;
        return JDN;
    }
    const calculateJD = (y, m, d, hr, mn, sc) => toJulianDate(y, m, d) - 0.5 + (hr + mn / 60 + sc / 3600) / 24;
    let JDN = toJulianDate(full_Year, month, day);
    let JD = calculateJD(full_Year, month, day, utc_hour, utc_minute, utc_second);
    let jCent = (JD - 2451545) / 36525;
    const rad = (g) => Math.PI * g / 180.0;
    const deg = (rd) => 180.0 * rd / Math.PI;
    let geomMeanLong = (280.46646 + (jCent * (36000.76983 + jCent * 0.0003032))) % 360.0;
    let geomMeanAnom = (357.52911 + jCent * (35999.05029 - 0.0001537 * jCent)) % 360.0;
    // Eccent of earth orbit
    let eccEO = 0.016708634 - jCent * (0.000042037 + 0.0000001267 * jCent);
    let gA = geomMeanAnom;
    let singA = Math.sin(rad(gA));
    let singA2 = Math.sin(2 * rad(gA));
    let singA3 = Math.sin(rad(3 * gA));
    let sunEqOfCtr = singA * (1.914602 - jCent * (0.004817 + 0.000014 * jCent))
        + singA2 * (0.019993 - 0.000101 * jCent)
        + singA3 * 0.000289;
    /*console.log("JD = ", JD, "JDN = ", JDN);
    console.log("geomMeanLong " + geomMeanLong);
    console.log("geomMeanAnom " + geomMeanAnom);
    console.log("eccEO " + eccEO);
    console.log("sunEqOfCtr " + sunEqOfCtr); */
    // sunTL is Sun true longitude composed here
    let sunTL = sunEqOfCtr + geomMeanLong;
    let sunAppLong = sunTL - 0.00569 - 0.00478 * Math.sin(rad((125.04 - 1934.136 * jCent)));
    let meanObliqEcliptic = 23 + (26 + (21.448 - jCent * (46.815 + jCent * (0.00059 - jCent * 0.001813))) / 60) / 60;
    let obliqCorr = meanObliqEcliptic + 0.00256 * Math.cos(rad(125.04 - 1934.136 * jCent));
    let sunDeclin = deg(Math.asin(Math.sin(rad(obliqCorr)) * Math.sin(rad(sunAppLong))));
    let y_var = Math.tan(rad(obliqCorr) / 2.0) ** 2;
    // Time Equation    
    let gmls = geomMeanLong;
    let gmas = geomMeanAnom;
    let y = y_var;
    let eot = y * Math.sin(2 * rad(gmls)) - 2 * eccEO * Math.sin(rad(gmas))
        + 4 * eccEO * y * Math.sin(rad(gmas)) * Math.cos(2 * rad(gmls))
        - 0.5 * y * y * Math.sin(4 * rad(gmls))
        - 1.25 * eccEO * eccEO * Math.sin(2 * rad(gmas));
    let eqTime = deg(eot) * 4.0; // Convert to minutes
    let latitude = cities[row].latitude;
    let longitude = cities[row].longitude;
    let timezone = cities[row].timezone;
    let dst_hour = 0;
    if (dts) {
        dst_hour = 1;
    }
    let tzOffset = timezone + dst_hour; // Adjust for daylight saving time
    let haSunrise = deg(Math.acos(Math.cos(rad(90.833)) / (Math.cos(rad(latitude)) * Math.cos(rad(sunDeclin)))
        - Math.tan(rad(latitude)) * Math.tan(rad(sunDeclin))));
    // Calculate the True Solar Time (in minutes)
    // time_in_minutes: local time in minutes (0..1440)
    let time_in_minutes = (utc_hour + timezone) * 60 + utc_minute + utc_second / 60;
    //  tz_offset from time.timezone is seconds west of UTC -> hours west of UTC
    //  Timezone convention: positive hours for east of Greenwich
    let true_solar_time = (time_in_minutes + eqTime + 4 * longitude - 60 * timezone) % 1440;
    // This converts time in minutes to format hours, minutes, seconds 
    function mins_to_hms(mins) {
        let h = Math.floor(mins / 60);
        let m = Math.floor(mins % 60);
        let s = Math.round(60 * (mins - Math.floor(mins)));
        return [h, m, s];
    }
    /*console.log('Time Equation ' + eqTime + ' minutes');
    console.log('haSunrise ' + haSunrise);
    console.log('true_solar_time' + true_solar_time); */
    function zpad(n) {
        let zn = String(n);
        if (n < 10) {
            zn = '0' + zn;
        }
        ;
        return zn;
    }
    const padTime = (hr, mn, sc) => zpad(hr) + ':' + zpad(mn) + ':' + zpad(sc);
    // Next is Solar Noontime
    let solar_noon = (720 - 4 * longitude - eqTime + 60 * tzOffset) % 1440;
    let [h, m, s] = mins_to_hms(solar_noon);
    let hours = h;
    let minutes = m;
    let seconds = s;
    //console.log("Solar Noon " + hours + ":" + minutes + ":" + seconds);
    let noonText = "<br>Solar Noon " + padTime(hours, minutes, seconds);
    let sunrise_time = solar_noon - haSunrise * 4; // in minutes
    let sunset_time = solar_noon + haSunrise * 4; // in minutes
    let dayLength = sunset_time - sunrise_time;
    if (sunset_time < sunrise_time) {
        dayLength += 1440;
    }
    // Calculate the solar zenith angle and solar elevation angle for the given time
    let solar_zenith_angle = deg(Math.acos(Math.sin(rad(latitude)) * Math.sin(rad(sunDeclin))
        + Math.cos(rad(latitude)) * Math.cos(rad(sunDeclin)) * Math.cos(rad(true_solar_time / 4 - 180))));
    let solar_elevation_angle = 90 - solar_zenith_angle;
    let elevationText = "Solar Elevation " + solar_elevation_angle.toFixed(3)
        + '° (without refraction correction)';
    let [sd_h, sd_m, sd_s] = mins_to_hms(dayLength);
    let [sr_h, sr_m, sr_s] = mins_to_hms(sunrise_time);
    let [ss_h, ss_m, ss_s] = mins_to_hms(sunset_time);
    let sunriseText = "Sunrise time " + padTime(sr_h, sr_m, sr_s);
    let sunsetText = "Sunset time " + padTime(ss_h, ss_m, ss_s);
    let dayLengthText = "Sunlight duration " + padTime(sd_h, sd_m, sd_s);
    // A simple atmospheric refraction correction for the solar elevation angle
    // It needs helper functions:
    // Three categories of elevations angle: < 0, < 5, < 85
    // used for refraction angles
    const belowZero = (hx) => -20.774 / Math.tan(rad(hx)) / 3600.0;
    const belowFive = (hx) => {
        return (1735.0 - 518.2 * hx + 103.4 * hx ** 2
            - 12.79 * hx ** 3 + 0.711 * hx ** 4) / 3600;
    };
    function belowEightyFive(hx) {
        let v = (58.1 / Math.tan(rad(hx)) - 0.07 / Math.tan(rad(hx)) ** 3
            + 0.000086 / Math.tan(rad(hx)) ** 5) / 3600;
        return v;
    }
    function atmosRefract(h) {
        let res;
        if (h < -0.575) {
            res = belowZero(h);
        }
        else if (h <= 5.0) {
            res = belowFive(h);
        }
        else if (h <= 85.0) {
            res = belowEightyFive(h);
        }
        else {
            res = 0.0;
        }
        return res;
    }
    function hourAngle(trueSolarTime) {
        let tst = trueSolarTime / 4.0;
        let res = tst;
        if (tst < 0) {
            res = tst + 180.0;
        }
        if (tst > 0) {
            res = tst - 180.0;
        }
        return res;
    }
    let ha = hourAngle(true_solar_time);
    //console.log("hourAngle " + ha.toFixed);
    function calcAzimuth(hourAngle, zenith, sunDeclin, latit) {
        let radZenith = rad(zenith);
        let radLatit = rad(latit);
        let radS = rad(sunDeclin);
        let numerator = Math.sin(radLatit) * Math.cos(radZenith) - Math.sin(radS);
        let denominator = Math.cos(radLatit) * Math.sin(radZenith);
        let acosValue = Math.acos(numerator / denominator);
        let degreesValue = acosValue * 180 / Math.PI;
        if (hourAngle > 0) {
            return (degreesValue + 180) % 360;
        }
        else {
            return (540 - degreesValue) % 360;
        }
    }
    let azimuth_angle = calcAzimuth(ha, solar_zenith_angle, sunDeclin, latitude);
    //console.log("azimuth_angle " + azimuth_angle.toFixed(4));
    let azimuthText = "Azimuth angle " + azimuth_angle.toFixed(3) + '°';
    let refraction_correction = atmosRefract(solar_elevation_angle);
    let solar_elevation_angle_corrected = solar_elevation_angle + refraction_correction;
    let refraction_corrected_elevation_text = "Solar elevation " + solar_elevation_angle_corrected.toFixed(3)
        + "° with atmospheric refr. correction " + refraction_correction.toFixed(3) + '°';
    
// Calculate the distance from Earth to Sun
let sunTrueAnom = geomMeanAnom + sunEqOfCtr;
let sunDistance = (1.000001018 * (1 - eccEO * eccEO)) / (1 + eccEO * Math.cos(rad(sunTrueAnom)));
let asMillionKM = 149.5978707 * sunDistance;
let asMillionMiles = 0.62137119223733 * asMillionKM;
console.log(`Sun Distance from Earth is ${sunDistance} AU`);
console.log(`Equal to ${asMillionKM.toFixed(3)} km`);
console.log("sunTrueAnom " + sunTrueAnom.toFixed(4));

    text += noonText + "<br>"
        + sunriseText + "<br>"
        + sunsetText + "<br>"
        + dayLengthText + "<br>"
        + "<br>" + elevationText + "<br>"
        + refraction_corrected_elevation_text + "<br>"
        + "<p>" + azimuthText + "</p>"
        + "Sun Distance from Earth is today " + sunDistance.toFixed(6) + " AU<br>"
        +  "That is equal to " + asMillionKM.toFixed(3) + " million km ("
        + asMillionMiles.toFixed(3) + " million miles)<br></div>";

    text += "<p>JDN " + JDN + ", JD " + JD.toFixed(6) + "</p>";
    text += "Solar Declination " + sunDeclin.toFixed(3) + '°';
    document.getElementById("results").innerHTML = '<br>City ' + name
        + '<br> Latitude ' + lat + ' Longitude ' + lon + ' Timezone ' + tz + 'h'
        + '<br>' + text;
    text += "<p>JDN " + JDN + ", JD " + JD.toFixed(6) + "</p>";
    text += "<p>Solar Declination " + sunDeclin.toFixed(3) + "°</p>";
}
const cities = [
    { city: "Helsinki", latitude: 60.1695, longitude: 24.9354, timezone: 2, timeZoneID: "Europe/Helsinki" },
    { city: "London", latitude: 51.5074, longitude: -0.1278, timezone: 0, timeZoneID: "Europe/London" },
    { city: "Stockholm", latitude: 59.3293, longitude: 18.0686, timezone: 1, timeZoneID: "Europe/Stockholm" },
    { city: "Oslo", latitude: 59.9139, longitude: 10.7522, timezone: 1, timeZoneID: "Europe/Oslo" },
    { city: "Berlin", latitude: 52.5200, longitude: 13.4050, timezone: 1, timeZoneID: "Europe/Berlin" },
    { city: "München", latitude: 48.1380, longitude: 11.5750, timezone: 1, timeZoneID: "Europe/Berlin" },
    { city: "Wien", latitude: 48.2195, longitude: 16.3785, timezone: 1, timeZoneID: "Europe/Vienna" },
    { city: "Zürich", latitude: 47.3775, longitude: 8.49540, timezone: 1, timeZoneID: "Europe/Zurich" },
    { city: "Geneve", latitude: 46.2040, longitude: 6.14300, timezone: 1, timeZoneID: "Europe/Zurich" },
    { city: "Paris", latitude: 48.8555, longitude: 2.34880, timezone: 1, timeZoneID: "Europe/Paris" },
    { city: "Brussels", latitude: 50.8524, longitude: 4.34180, timezone: 1, timeZoneID: "Europe/Brussels" },
    { city: "New York", latitude: 40.7128, longitude: -74.0059, timezone: -5, timeZoneID: "America/New_York" },
    { city: "Washington D.C.", latitude: 38.9050, longitude: -77.0160, timezone: -5, timeZoneID: "America/New_York" },
    { city: "Champaign IL", latitude: 40.1200, longitude: -88.2400, timezone: -6, timeZoneID: "America/Chicago" },
    { city: "Anchorage Alaska", latitude: 61.1830, longitude: -149.883, timezone: -9, timeZoneID: "America/Anchorage" },
    { city: "Vancouver B.C.", latitude: 49.2820, longitude: -123.120, timezone: -8, timeZoneID: "Canada/Pacific" },
    { city: "Madrid", latitude: 40.4190, longitude: -3.6930, timezone: 1, timeZoneID: "Europe/Madrid" },
    { city: "Malaga", latitude: 36.7200, longitude: -4.4150, timezone: 1, timeZoneID: "Europe/Madrid" },
    { city: "Barcelona", latitude: 41.3860, longitude: 2.17300, timezone: 1, timeZoneID: "Europe/Madrid" },
    { city: "Murcia", latitude: 37.9880, longitude: -1.1330, timezone: 1, timeZoneID: "Europe/Madrid" },
    { city: "Kemi", latitude: 65.7360, longitude: 24.5560, timezone: 2, timeZoneID: "Europe/Helsinki" },
    { city: "Tornio", latitude: 65.8480, longitude: 24.1446, timezone: 2, timeZoneID: "Europe/Helsinki" },
    { city: "Oulu", latitude: 65.0140, longitude: 25.4730, timezone: 2, timeZoneID: "Europe/Helsinki" },
    { city: "Rovaniemi", latitude: 66.5020, longitude: 25.7240, timezone: 2, timeZoneID: "Europe/Helsinki" },
    { city: "Utsjoki", latitude: 69.90954, longitude: 27.0295, timezone: 2, timeZoneID: "Europe/Helsinki" },
    { city: "Tokyo", latitude: 35.7000, longitude: 139.7700, timezone: 9, timeZoneID: "Asia/Tokyo" },
    { city: "Sydney AUS", latitude: -33.870, longitude: 151.2200, timezone: 10, timeZoneID: "Australia/Sydney" }
];
