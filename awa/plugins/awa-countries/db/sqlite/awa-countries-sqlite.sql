/* File generated automatically by dynamo */
/*  */
CREATE TABLE awa_city (
  /* the city identifier */
  `id` INTEGER PRIMARY KEY,
  /* the city name */
  `name` VARCHAR(255) NOT NULL,
  /* the city ZIP code */
  `zip_code` INTEGER NOT NULL,
  /* the city latitude */
  `latitude` INTEGER NOT NULL,
  /* the city longitude */
  `longitude` INTEGER NOT NULL,
  /* the region that this city belongs to */
  `region_id` INTEGER NOT NULL,
  /* the country that this city belongs to */
  `country_id` INTEGER NOT NULL
);
/* The country model is a system data model for the application.
In theory, it never changes. */
CREATE TABLE awa_country (
  /* the country identifier */
  `id` INTEGER PRIMARY KEY,
  /* the country name */
  `name` VARCHAR(255) NOT NULL,
  /* the continent name */
  `continent` VARCHAR(255) NOT NULL,
  /* the currency used in the country */
  `currency` VARCHAR(255) NOT NULL,
  /* the country ISO code */
  `iso_code` VARCHAR(255) NOT NULL,
  /* the country geoname id */
  `geonameid` INTEGER NOT NULL,
  /* the country main language */
  `languages` VARCHAR(255) NOT NULL,
  /* the TLD associated with this country */
  `tld` VARCHAR(3) NOT NULL,
  /* the currency code */
  `currency_code` VARCHAR(3) NOT NULL
);
/* The country neighbor defines what countries
are neigbors with each other */
CREATE TABLE awa_country_neighbor (
  /*  */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `neighbor_of_id` INTEGER NOT NULL,
  /*  */
  `neighbor_id` INTEGER NOT NULL
);
/* Region defines an area within a country. */
CREATE TABLE awa_region (
  /* the region identifier */
  `id` INTEGER PRIMARY KEY,
  /* the region name */
  `name` VARCHAR(255) NOT NULL,
  /* the region geonameid */
  `geonameid` INTEGER NOT NULL,
  /* the country that this region belongs to */
  `country_id` INTEGER NOT NULL
);
INSERT INTO entity_type (name) VALUES ("awa_city");
INSERT INTO entity_type (name) VALUES ("awa_country");
INSERT INTO entity_type (name) VALUES ("awa_country_neighbor");
INSERT INTO entity_type (name) VALUES ("awa_region");
