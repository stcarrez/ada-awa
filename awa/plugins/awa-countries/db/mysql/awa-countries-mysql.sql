/* File generated automatically by dynamo */
/*  */
CREATE TABLE awa_city (
  /*  */
  `id` INTEGER NOT NULL,
  /* the city name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the city ZIP code */
  `zip_code` INTEGER NOT NULL,
  /*  */
  `latitude` INTEGER NOT NULL,
  /*  */
  `longitude` INTEGER NOT NULL,
  /*  */
  `region_id` INTEGER NOT NULL,
  /*  */
  `country_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
/* The country model is a system data model for the application.
In theory, it never changes. */
CREATE TABLE awa_country (
  /* the country identifier */
  `id` INTEGER NOT NULL,
  /* the country name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the continent name */
  `continent` VARCHAR(255) BINARY NOT NULL,
  /* the currency used in the country */
  `currency` VARCHAR(255) BINARY NOT NULL,
  /* the country ISO code */
  `iso_code` VARCHAR(255) BINARY NOT NULL,
  /* the country geoname id */
  `geonameid` INTEGER NOT NULL,
  /* the country main language */
  `languages` VARCHAR(255) BINARY NOT NULL,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE awa_country_neighbor (
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `neighbor_of_id` INTEGER NOT NULL,
  /*  */
  `neighbor_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE awa_region (
  /* the region identifier */
  `id` INTEGER NOT NULL,
  /* the region name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the region geonameid */
  `geonameid` INTEGER NOT NULL,
  /*  */
  `country_id` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("awa_city")
,("awa_country")
,("awa_country_neighbor")
,("awa_region")
;
