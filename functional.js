// @ts-ignore
"use strict";
// The goal of this version:
// 1.) To make it more functional using more functions and less global variables
//     As many functions have jCent as primary variable, many other variables may be removed.

// 2.) Usage of the library Luxon to replace some of my methods and functions.
// Using luxon.js and TZ names we'll get allways the right time zone offset and the state of DST.

import { DateTime } from "https://cdn.jsdelivr.net/npm/luxon@3.4.4/+esm";

/*
 * @typedef {object} City
 * @property {string} city
 * @property {number} latitude
 * @property {number} longitude
 * @property {number} timezone
 * @property {string} timeZoneID
 */

/*
 * @param {number} offset
 * @returns {string}
 */
// create Date object for current location


const localize = (dt, zone, locale) => {
  return dt.setZone(zone)
    .setLocale(locale)
    .toLocaleString(DateTime.DATETIME_FULL);
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
    //let tz = cities[row].timezone;
    let tzName = cities[row].timeZoneID;
    const dh = DateTime.now();
    const local_time = new Date();
    const local_time_string = local_time.toString();
    const utc_date = DateTime.now().toUTC().toISODate();
    const utc_time = DateTime.now().toUTC().toISOTime();
    // Get current date and time UTC
    const currentDt = DateTime.now();
    const currentUTC = DateTime.now().toUTC().toISO({precision: 'second'});
    const dateCity = DateTime.fromISO(currentUTC).setZone(tzName);
    console.log('dateCity.hour', dateCity.hour);
    console.log('ISO UTC String', currentUTC);
    console.log(tzName, dateCity.toString());
    console.log('Local String', currentDt.toISO());
    console.log("Year: ", currentDt.year);
    console.log("Month: ", currentDt.month);
    console.log("Day: ", currentDt.day);
    console.log("Hour: ", currentDt.hour);
    console.log("Minute: ", currentDt.minute);
    console.log("Second: ", currentDt.second);
    console.log("Time Zone Name: ", currentDt.zoneName);
    console.log("Time Zone Offset: ", currentDt.offset); 
    console.log('Is this UTC time?', currentUTC);
    console.log('Other city time', tzName, dateCity.offset, dateCity.toISO({precision: 'second'})); 
    let text = "UTC date and time " + utc_date + ' <> ' + utc_time.toString().slice(0, -5);
    const d = currentDt;
    const hour = d.hour;
    const minute = d.minute;
    const second = d.second;
    const day = currentDt.day;
    const month = 1 + currentDt.month;
    const year = d.year;
    console.log('Helsinki ', localize(dh, 'Europe/Helsinki', 'fi'));    
    //let dts = true; // Daylight saving
    let otherCity = cities[row].city;
    //if (otherCity == 'Tokyo') { dts = false;}; // No DTS in Japan!
    //if ((cities[row].latitude < 0) && (month > 3) && (month < 10)) {dts = false;}
    let otherLang = cities[row].language;
    // Add DTS hour to timezone offset in northern world:
    //if ((dts) && (cities[row].latitude > 0)) { otherOffset += 1;};
    // Add DTS hour to timezone offset in southern world
    //if ((dts) && (cities[row].latitude < 0)) {otherOffset += 1;};
    //let otherTime = calcTime(otherOffset);
    let otherTime = localize(dh, cities[row].timeZoneID, otherLang);
    const dto = dh.setZone(cities[row].timeZoneID);
    let offset = dto.offset;
    let otherHour = dateCity.hour;
    text += "<br>" + otherCity + ": " + otherTime + " ( UTC + " + offset/60 + ' h )';

    text += "<br>Your local time now: " + local_time_string.slice(3, 34);
    
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
    const calculateJD = (y, m, d, hr, mn, sc) => { console.log(y, m, d, hr, mn, sc);
        return toJulianDate(y, m, d) - 0.5 + (hr + mn / 60 + sc / 3600) / 24;}
    let JDN = toJulianDate(year, month - 1, day);
    let JD = calculateJD(year, month - 1, day, hour - 3, minute, second);
    let jCent = (JD - 2451545) / 36525;
    const rad = (g) => Math.PI * g / 180.0;
    const deg = (rd) => 180.0 * rd / Math.PI;
   
    // Calculate the date of the September change DST
    const JDN_End_DST = (year) => toJulianDate(year, 10, 31);

    console.log("End DST (time 1 h backward) on " + (31 - JDN_End_DST(year) % 7 - 1) + ".10." + year);
    // End DST on 25.10.2026 European countries
   
    function geomMeanLong(jc) {
        let gML = (280.46646 + (jc * (36000.76983 + jc * 0.0003032))) % 360.0;
        return gML;
    }


    function geomMeanAnom(jc) {
        let gMA = (357.52911 + jc * (35999.05029 - 0.0001537 * jc)) % 360.0;
        return gMA;
    }

    // Eccent of earth orbit
    function acentricOrbit(jc) {
        const eccEO = 0.016708634 - jc * (0.000042037 + 0.0000001267 * jc);
        return eccEO;
    }

    let eccEO = acentricOrbit(jCent);
    
    function sunEqOfCtr(jc) {
        let gMA = geomMeanAnom(jc);
        let sunEoC = Math.sin(rad(gMA)) * (1.914602 - jc * (0.004817 + 0.000014 * jc))
            + Math.sin(2 * rad(gMA)) * (0.019993 - 0.000101 * jc)
            + Math.sin(3 * rad(gMA)) * 0.000289;
        return sunEoC;
    }


    function obliqCorr(jc) {
        const meanObliqEcliptic = 23 + (26 + (21.448 - jc * (46.815 + jc * (0.00059 - jc * 0.001813))) / 60) / 60;
        const obCorr = meanObliqEcliptic + 0.00256 * Math.cos(rad(125.04 - 1934.136 * jc));
        return obCorr;
    }

    function sunDeclin(jc) {
        // sunTL is Sun true longitude 
        const sunTL = sunEqOfCtr(jc) + geomMeanLong(jc);
        const sunAppLong = sunTL - 0.00569 - 0.00478 * Math.sin(rad((125.04 - 1934.136 * jc)));
        const decl = deg(Math.asin(Math.sin(rad(obliqCorr(jc))) * Math.sin(rad(sunAppLong))));
        return decl;
    }

    const maxAltitude = (latit, declin) => 90 - Math.abs(latit - declin);

    // Time Equation    
    function timeEquation(jc) {
        const y_var = Math.tan(rad(obliqCorr(jc)) / 2.0) ** 2;
        const gML = geomMeanLong(jc);
        const gMA = geomMeanAnom(jc);
        const eccEO = acentricOrbit(jc);
        const eot = y_var * Math.sin(2 * rad(gML)) - 2 * eccEO * Math.sin(rad(gMA))
          + 4 * eccEO * y_var * Math.sin(rad(gMA)) * Math.cos(2 * rad(gML))
          - 0.5 * y_var * y_var * Math.sin(4 * rad(gML))
          - 1.25 * eccEO * eccEO * Math.sin(2 * rad(gMA));
        const eqTime = deg(eot) * 4.0; // Convert to minutes
        return eqTime;
    }

    let latitude = cities[row].latitude;
    let longitude = cities[row].longitude;
    //let dst_hour = 0;
    //if (dts) {
    //    dst_hour = 1;
    //}
    //let tzOffset = timezone + dst_hour; // Adjust for daylight saving time
    let haSunrise = deg(Math.acos(Math.cos(rad(90.833)) / (Math.cos(rad(latitude)) * Math.cos(rad(sunDeclin(jCent))))
        - Math.tan(rad(latitude)) * Math.tan(rad(sunDeclin(jCent)))));
    // Calculate the True Solar Time (in minutes)
    // time_in_minutes: time in minutes (0..1440)
    let time_in_minutes = (otherHour) * 60 + minute + second / 60;
    console.log('Time in minutes using hour, min, second', otherHour, minute, second);
    //  tz_offset from time.timezone is seconds west of UTC -> hours west of UTC
    //  Timezone convention: positive hours for east of Greenwich
    //console.log('r213 tark. zone', zone); // p.o. utc
    // This converts time in minutes to format hours, minutes, seconds 
    function mins_to_hms(mins) {
        let h = Math.floor(mins / 60);
        let m = Math.floor(mins % 60);
        let s = Math.round(60 * (mins - Math.floor(mins)));
        return [h, m, s];
    }
    
  
    const kurzeZeit = (zeitNummer) => new Date(60000 * zeitNummer).toISOString().slice(11,-5);  

    // Next is Solar Noontime
    console.log('tarkistus offset',offset,'minutes');
    const solar_noon = (720 - 4 * longitude - timeEquation(jCent) + offset) % 1440;
    const noonString = kurzeZeit(solar_noon);
    let decl = sunDeclin(jCent);
    let noonText = "<br>Solar Noon\t" + noonString + ', max altitude '
     +  maxAltitude(latitude, decl).toFixed(2) + '°';
    let sunrise_time = solar_noon - haSunrise * 4; // in minutes
    let sunset_time = solar_noon + haSunrise * 4; // in minutes
    let dayLength = sunset_time - sunrise_time; // in minutes
    if (sunset_time < sunrise_time) {
        dayLength += 1440;
    }
    // Calculate the solar zenith angle and solar elevation angle for the given time
    console.log('Time in minutes', time_in_minutes, 'Longitude', longitude, 'Timezone', dateCity.offset); 
    let true_solar_time = (time_in_minutes + timeEquation(jCent) + 4 * longitude - dateCity.offset) % 1440;
    if (true_solar_time < 0) {true_solar_time += 1440}; // make allways positive minutes
    console.log('Time in minutes', time_in_minutes, 'True Solar Time', true_solar_time);
    console.log('true_solar_time', true_solar_time);
    let solar_zenith_angle = deg(Math.acos(Math.sin(rad(latitude)) * Math.sin(rad(sunDeclin(jCent)))
        + Math.cos(rad(latitude)) * Math.cos(rad(sunDeclin(jCent))) * Math.cos(rad(true_solar_time / 4 - 180))));
    let solar_elevation_angle = 90 - solar_zenith_angle;
    let elevationText = "Solar Elevation " + solar_elevation_angle.toFixed(3)
        + '° (without refraction correction)';
    let dayLengthString = kurzeZeit(dayLength);
    let sunriseString   = kurzeZeit(sunrise_time);
    let sunsetString    = kurzeZeit(sunset_time); 
    let sunriseText = "Sunrise time\t" + sunriseString;
    let sunsetText = "Sunset time\t" + sunsetString;
    let dayLengthText = "Sunlight duration\t" + dayLengthString;
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
        console.log('True Solar Time', trueSolarTime, 'Time in minutes', time_in_minutes);
        return res;
    }
    let ha = hourAngle(true_solar_time);
    //console.log("hourAngle " + ha.toFixed);
    function calcAzimuth(hourAngle, zenith, jc, latit) {
        let radZenith = rad(zenith);
        let radLatit = rad(latit);
        let radS = rad(sunDeclin(jc));
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
    // Solar altitude angle at noon (max altitude of the day):
    // Noon Sun Angle = 90° - [Latitude - Solar Declination]
    let azimuth_angle = calcAzimuth(ha, solar_zenith_angle, jCent, latitude);
    let azimuthText = "Azimuth angle " + azimuth_angle.toFixed(3) + '°';
    let refraction_correction = atmosRefract(solar_elevation_angle);
    let solar_elevation_angle_corrected = solar_elevation_angle + refraction_correction;
    let refraction_corrected_elevation_text = "Solar elevation " + solar_elevation_angle_corrected.toFixed(3)
        + "° with atmospheric refr. correction " + refraction_correction.toFixed(3) + '°';
    
// Calculate the distance from Earth to Sun
let sunTrueAnom = geomMeanAnom(jCent) + sunEqOfCtr(jCent);
let sunDistance = (1.000001018 * (1 - eccEO * eccEO)) / (1 + eccEO * Math.cos(rad(sunTrueAnom)));
let asMillionKM = 149.5978707 * sunDistance;
let asMillionMiles = 0.62137119223733 * asMillionKM;
console.log(`Sun Distance from Earth is ${sunDistance} AU`);
console.log(`Equal to ${asMillionKM.toFixed(3)} km`);

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
    text += "Solar Declination " + sunDeclin(jCent).toFixed(3) + '°';
    document.getElementById("results").innerHTML = '<br>City ' + name
        + '<br> Latitude ' + lat + ' Longitude ' + lon 
        + '<br>' + text;
    text += "<p>JDN " + JDN + ", JD " + JD.toFixed(6) + "</p>";
    text += "<p>Solar Declination " + sunDeclin(jCent).toFixed(3) + "°</p>";
}
const cities = [
    { city: "Helsinki", latitude: 60.1695, longitude: 24.9354, language: 'fi-FI', timeZoneID: "Europe/Helsinki" },
    { city: "London", latitude: 51.5074, longitude: -0.1278, language: 'en-GB', timeZoneID: "Europe/London" },
    { city: "Stockholm", latitude: 59.3293, longitude: 18.0686, language: 'sv', timeZoneID: "Europe/Stockholm" },
    { city: "Oslo", latitude: 59.9139, longitude: 10.7522, language: 'no-NO', timeZoneID: "Europe/Oslo" },
    { city: "Berlin", latitude: 52.5200, longitude: 13.4050, language: 'de-DE', timeZoneID: "Europe/Berlin" },
    { city: "München", latitude: 48.1380, longitude: 11.5750, language: 'de-DE', timeZoneID: "Europe/Berlin" },
    { city: "Wien", latitude: 48.2195, longitude: 16.3785, language: 'de-OE', timeZoneID: "Europe/Vienna" },
    { city: "Zürich", latitude: 47.3775, longitude: 8.49540, language: 'de-CH', timeZoneID: "Europe/Zurich" },
    { city: "Geneve", latitude: 46.2040, longitude: 6.14300, language: 'de-CH', timeZoneID: "Europe/Zurich" },
    { city: "Paris", latitude: 48.8555, longitude: 2.34880, language: 'fr-FR', timeZoneID: "Europe/Paris" },
    { city: "Brussels", latitude: 50.8524, longitude: 4.34180, language: 'en-BE', timeZoneID: "Europe/Brussels" },
    { city: "New York", latitude: 40.7128, longitude: -74.0059, language: 'en-US', timeZoneID: "America/New_York" },
    { city: "Washington D.C.", latitude: 38.9050, longitude: -77.0160, language: 'en-US', timeZoneID: "America/New_York" },
    { city: "Champaign IL", latitude: 40.1200, longitude: -88.2400, language: 'en-US', timeZoneID: "America/Chicago" },
    { city: "Anchorage Alaska", latitude: 61.1830, longitude: -149.883, language: 'en-US', timeZoneID: "America/Anchorage" },
    { city: "Vancouver B.C.", latitude: 49.2820, longitude: -123.120, language: 'en-US', timeZoneID: "Canada/Pacific" },
    { city: "Madrid", latitude: 40.4190, longitude: -3.6930, language: 'es-ES', timeZoneID: "Europe/Madrid" },
    { city: "Malaga", latitude: 36.7200, longitude: -4.4150, language: 'es-ES', timeZoneID: "Europe/Madrid" },
    { city: "Barcelona", latitude: 41.3860, longitude: 2.17300, language: 'es-ES', timeZoneID: "Europe/Madrid" },
    { city: "Murcia", latitude: 37.9880, longitude: -1.1330, language: 'es-ES', timeZoneID: "Europe/Madrid" },
    { city: "Kemi", latitude: 65.7360, longitude: 24.5560, language: 'fi-FI', timeZoneID: "Europe/Helsinki" },
    { city: "Tornio", latitude: 65.8480, longitude: 24.1446, language: 'fi-FI', timeZoneID: "Europe/Helsinki" },
    { city: "Oulu", latitude: 65.0140, longitude: 25.4730, language: 'fi-FI', timeZoneID: "Europe/Helsinki" },
    { city: "Rovaniemi", latitude: 66.5020, longitude: 25.7240, language: 'fi-FI', timeZoneID: "Europe/Helsinki" },
    { city: "Utsjoki", latitude: 69.90954, longitude: 27.0295, language: 'fi-FI', timeZoneID: "Europe/Helsinki" },
    { city: "Tokyo", latitude: 35.7000, longitude: 139.7700, language: 'ja-JP', timeZoneID: "Asia/Tokyo" },
    { city: "Sydney AUS", latitude: -33.870, longitude: 151.2200, language: 'en-AU', timeZoneID: "Australia/Sydney" }
];

document.querySelector("#runButton").addEventListener("click", getOption);
