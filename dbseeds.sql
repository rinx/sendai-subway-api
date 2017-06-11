create schema ssubapi;

create table ssubapi.line (
    id serial unique primary key,
    name varchar(255) not null);
create table ssubapi.station (
    id integer unique not null primary key,
    name varchar(255) not null);
create table ssubapi.destination (
    id integer unique not null primary key,
    name varchar(255) not null);
create table ssubapi.line_station (
    id serial unique primary key,
    line_id integer not null,
    station_id integer not null);
create table ssubapi.station_destination (
    id serial unique primary key,
    station_id integer not null,
    destination_id integer not null);

create table ssubapi.timetable (
    id serial unique primary key,
    station_destination_id integer not null,
    train_time time not null);

insert into ssubapi.line (id, name) values (1, '南北線');
insert into ssubapi.line (id, name) values (2, '東西線');

insert into ssubapi.destination (id, name) values (0, '富沢行');
insert into ssubapi.destination (id, name) values (1, '泉中央行');
insert into ssubapi.destination (id, name) values (2, '荒井行');
insert into ssubapi.destination (id, name) values (3, '八木山動物公園行');

insert into ssubapi.station (id, name) values (9285, '仙台');
insert into ssubapi.line_station (station_id, destination_id) values (1,9285);
insert into ssubapi.line_station (station_id, destination_id) values (2,9285);
insert into ssubapi.station_destination (station_id, destination_id) values (9285, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (9285, 1);
insert into ssubapi.station_destination (station_id, destination_id) values (9285, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9285, 3);

insert into ssubapi.station (id, name) values (6646, '泉中央');
insert into ssubapi.line_station (station_id, destination_id) values (1,6646);
insert into ssubapi.station_destination (station_id, destination_id) values (6646, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6646, 1);

insert into ssubapi.station (id, name) values (6647, '八乙女');
insert into ssubapi.line_station (station_id, destination_id) values (1,6647);
insert into ssubapi.station_destination (station_id, destination_id) values (6647, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6647, 1);

insert into ssubapi.station (id, name) values (6648, '黒松');
insert into ssubapi.line_station (station_id, destination_id) values (1,6648);
insert into ssubapi.station_destination (station_id, destination_id) values (6648, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6648, 1);

insert into ssubapi.station (id, name) values (6649, '旭ケ丘');
insert into ssubapi.line_station (station_id, destination_id) values (1,6649);
insert into ssubapi.station_destination (station_id, destination_id) values (6649, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6649, 1);

insert into ssubapi.station (id, name) values (6650, '台原');
insert into ssubapi.line_station (station_id, destination_id) values (1,6650);
insert into ssubapi.station_destination (station_id, destination_id) values (6650, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6650, 1);

insert into ssubapi.station (id, name) values (1222, '北仙台');
insert into ssubapi.line_station (station_id, destination_id) values (1,1222);
insert into ssubapi.station_destination (station_id, destination_id) values (1222, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (1222, 1);

insert into ssubapi.station (id, name) values (6651, '北四番丁');
insert into ssubapi.line_station (station_id, destination_id) values (1,6651);
insert into ssubapi.station_destination (station_id, destination_id) values (6651, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6651, 1);

insert into ssubapi.station (id, name) values (6652, '勾当台公園');
insert into ssubapi.line_station (station_id, destination_id) values (1,6652);
insert into ssubapi.station_destination (station_id, destination_id) values (6652, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6652, 1);

insert into ssubapi.station (id, name) values (6653, '広瀬通');
insert into ssubapi.line_station (station_id, destination_id) values (1,6653);
insert into ssubapi.station_destination (station_id, destination_id) values (6653, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6653, 1);

insert into ssubapi.station (id, name) values (6654, '五橋');
insert into ssubapi.line_station (station_id, destination_id) values (1,6654);
insert into ssubapi.station_destination (station_id, destination_id) values (6654, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6654, 1);

insert into ssubapi.station (id, name) values (6655, '愛宕橋');
insert into ssubapi.line_station (station_id, destination_id) values (1,6655);
insert into ssubapi.station_destination (station_id, destination_id) values (6655, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6655, 1);

insert into ssubapi.station (id, name) values (6656, '河原町');
insert into ssubapi.line_station (station_id, destination_id) values (1,6656);
insert into ssubapi.station_destination (station_id, destination_id) values (6656, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6656, 1);

insert into ssubapi.station (id, name) values (6657, '長町一丁目');
insert into ssubapi.line_station (station_id, destination_id) values (1,6657);
insert into ssubapi.station_destination (station_id, destination_id) values (6657, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6657, 1);

insert into ssubapi.station (id, name) values (975, '長町');
insert into ssubapi.line_station (station_id, destination_id) values (1,975);
insert into ssubapi.station_destination (station_id, destination_id) values (975, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (975, 1);

insert into ssubapi.station (id, name) values (6658, '長町南');
insert into ssubapi.line_station (station_id, destination_id) values (1,6658);
insert into ssubapi.station_destination (station_id, destination_id) values (6658, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6658, 1);

insert into ssubapi.station (id, name) values (6659, '富沢');
insert into ssubapi.line_station (station_id, destination_id) values (1,6659);
insert into ssubapi.station_destination (station_id, destination_id) values (6659, 0);
insert into ssubapi.station_destination (station_id, destination_id) values (6659, 1);

insert into ssubapi.station (id, name) values (9727, '八木山動物公園');
insert into ssubapi.line_station (station_id, destination_id) values (2,9727);
insert into ssubapi.station_destination (station_id, destination_id) values (9727, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9727, 3);

insert into ssubapi.station (id, name) values (9728, '青葉山');
insert into ssubapi.line_station (station_id, destination_id) values (2,9728);
insert into ssubapi.station_destination (station_id, destination_id) values (9728, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9728, 3);

insert into ssubapi.station (id, name) values (9729, '川内');
insert into ssubapi.line_station (station_id, destination_id) values (2,9729);
insert into ssubapi.station_destination (station_id, destination_id) values (9729, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9729, 3);

insert into ssubapi.station (id, name) values (9730, '国際センター');
insert into ssubapi.line_station (station_id, destination_id) values (2,9730);
insert into ssubapi.station_destination (station_id, destination_id) values (9730, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9730, 3);

insert into ssubapi.station (id, name) values (9731, '大町西公園');
insert into ssubapi.line_station (station_id, destination_id) values (2,9731);
insert into ssubapi.station_destination (station_id, destination_id) values (9731, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9731, 3);

insert into ssubapi.station (id, name) values (9732, '青葉通一番町');
insert into ssubapi.line_station (station_id, destination_id) values (2,9732);
insert into ssubapi.station_destination (station_id, destination_id) values (9732, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9732, 3);

insert into ssubapi.station (id, name) values (9733, '宮城野通');
insert into ssubapi.line_station (station_id, destination_id) values (2,9733);
insert into ssubapi.station_destination (station_id, destination_id) values (9733, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9733, 3);

insert into ssubapi.station (id, name) values (9734, '連坊');
insert into ssubapi.line_station (station_id, destination_id) values (2,9734);
insert into ssubapi.station_destination (station_id, destination_id) values (9734, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9734, 3);

insert into ssubapi.station (id, name) values (9735, '薬師堂');
insert into ssubapi.line_station (station_id, destination_id) values (2,9735);
insert into ssubapi.station_destination (station_id, destination_id) values (9735, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9735, 3);

insert into ssubapi.station (id, name) values (9736, '卸町');
insert into ssubapi.line_station (station_id, destination_id) values (2,9736);
insert into ssubapi.station_destination (station_id, destination_id) values (9736, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9736, 3);

insert into ssubapi.station (id, name) values (9737, '六丁の目');
insert into ssubapi.line_station (station_id, destination_id) values (2,9737);
insert into ssubapi.station_destination (station_id, destination_id) values (9737, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9737, 3);

insert into ssubapi.station (id, name) values (9738, '荒井');
insert into ssubapi.line_station (station_id, destination_id) values (2,9738);
insert into ssubapi.station_destination (station_id, destination_id) values (9738, 2);
insert into ssubapi.station_destination (station_id, destination_id) values (9738, 3);

