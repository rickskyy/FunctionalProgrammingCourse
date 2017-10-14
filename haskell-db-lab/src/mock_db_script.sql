CREATE TABLE sport_univ_teacher (
  id SERIAL PRIMARY KEY,
  name VARCHAR(20) NOT NULL,
  surname VARCHAR(20) NOT NULL
);

CREATE TABLE sport_univ_section (
  id SERIAL PRIMARY KEY,
  name VARCHAR(20) NOT NULL,
  teacherID INT REFERENCES sport_univ_teacher(id)
);

CREATE TABLE sport_univ_student (
  id SERIAL PRIMARY KEY,
  name VARCHAR(20) NOT NULL,
  surname VARCHAR(20) NOT NULL,
  sectionID INT REFERENCES sport_univ_section(id)
);

CREATE TABLE sport_univ_schedule (
  id SERIAL PRIMARY KEY,
  sectionID INT REFERENCES sport_univ_section(id) NOT NULL,
  beginDay VARCHAR(3) NOT NULL,
  beginTime TIME NOT NULL,
  endTime TIME NOT NULL
);

CREATE TABLE sport_univ_competition (
  id SERIAL PRIMARY KEY,
  sectionID SERIAL REFERENCES sport_univ_section(id) NOT NULL,
  beginTime TIMESTAMP NOT NULL,
  endTime TIMESTAMP NOT NULL,
  winner INT REFERENCES sport_univ_student(id)
);

INSERT INTO sport_univ_teacher (name, surname) VALUES ('Luis', 'Enrique');
INSERT INTO sport_univ_teacher (name, surname) VALUES ('Michael', 'Felps');
INSERT INTO sport_univ_teacher (name, surname) VALUES ('Arnold', 'Schwarzenegger');

INSERT INTO sport_univ_section (name, teacherID) VALUES ('Football', 1);
INSERT INTO sport_univ_section (name, teacherID) VALUES ('Swimming', 2);
INSERT INTO sport_univ_section (name, teacherID) VALUES ('Gym', 3);

INSERT INTO sport_univ_student (name, surname, sectionID) VALUES ('Rick', 'Sanchez',  1);
INSERT INTO sport_univ_student (name, surname, sectionID) VALUES ('Morty', 'Sanchez', 1);
INSERT INTO sport_univ_student (name, surname, sectionID) VALUES ('Ann', 'Vizz', 2);
INSERT INTO sport_univ_student (name, surname, sectionID) VALUES ('Jamie', 'Braun', 2);
INSERT INTO sport_univ_student (name, surname, sectionID) VALUES ('Luisa', 'Garfield', 3);
INSERT INTO sport_univ_student (name, surname, sectionID) VALUES ('Colton', 'Wise', 3);

INSERT INTO sport_univ_schedule (sectionID, beginDay, beginTime, endTime) VALUES (1, 'Mon', '16:00', '18:00');
INSERT INTO sport_univ_schedule (sectionID, beginDay, beginTime, endTime) VALUES (1, 'Trd', '16:00', '18:00');
INSERT INTO sport_univ_schedule (sectionID, beginDay, beginTime, endTime) VALUES (2, 'Wed', '16:00', '18:00');
INSERT INTO sport_univ_schedule (sectionID, beginDay, beginTime, endTime) VALUES (2, 'Frd', '17:00', '19:00');
INSERT INTO sport_univ_schedule (sectionID, beginDay, beginTime, endTime) VALUES (3, 'Tue', '18:00', '21:00');
INSERT INTO sport_univ_schedule (sectionID, beginDay, beginTime, endTime) VALUES (3, 'Sun', '19:00', '21:00');

INSERT INTO sport_univ_competition (sectionID, beginTime, endTime) VALUES  (1, '2017-11-04 14:00:00', '2017-11-04 20:00:00');
INSERT INTO sport_univ_competition (sectionID, beginTime, endTime) VALUES (1, '2017-11-05 16:00:00', '2017-11-04 20:00:00');
INSERT INTO sport_univ_competition (sectionID, beginTime, endTime) VALUES (1, '2017-11-07 12:00:00', '2017-11-04 20:00:00');