/* File generated automatically by dynamo */
/*  */
CREATE TABLE awa_city (
  /*  */
  `id` INTEGER PRIMARY KEY,
  /* the city name */
  `name` VARCHAR(255) NOT NULL,
  /* the city ZIP code */
  `zip_code` INTEGER NOT NULL,
  /*  */
  `latitude` INTEGER NOT NULL,
  /*  */
  `longitude` INTEGER NOT NULL,
  /*  */
  `region_id` INTEGER NOT NULL,
  /*  */
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
  /*  */
  `currency_code` VARCHAR(3) NOT NULL
);
/*  */
CREATE TABLE awa_country_neighbor (
  /*  */
  `id` BIGINT PRIMARY KEY,
  /*  */
  `neighbor_of_id` INTEGER NOT NULL,
  /*  */
  `neighbor_id` INTEGER NOT NULL
);
/*  */
CREATE TABLE awa_region (
  /* the region identifier */
  `id` INTEGER PRIMARY KEY,
  /* the region name */
  `name` VARCHAR(255) NOT NULL,
  /* the region geonameid */
  `geonameid` INTEGER NOT NULL,
  /*  */
  `country_id` INTEGER NOT NULL
);
INSERT INTO entity_type (name) VALUES ("awa_city");
INSERT INTO entity_type (name) VALUES ("awa_country");
INSERT INTO entity_type (name) VALUES ("awa_country_neighbor");
INSERT INTO entity_type (name) VALUES ("awa_region");
