/* File generated automatically by dynamo */
/*  */
CREATE TABLE IF NOT EXISTS awa_city (
  /* the city identifier */
  `id` BIGINT NOT NULL,
  /* the city name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the city ZIP code */
  `zip_code` INTEGER NOT NULL,
  /* the city latitude */
  `latitude` INTEGER NOT NULL,
  /* the city longitude */
  `longitude` INTEGER NOT NULL,
  /* the region that this city belongs to */
  `region_id` BIGINT NOT NULL,
  /* the country that this city belongs to */
  `country_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The country model is a system data model for the application.
In theory, it never changes. */
CREATE TABLE IF NOT EXISTS awa_country (
  /* the country identifier */
  `id` BIGINT NOT NULL,
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
  /* the TLD associated with this country */
  `tld` VARCHAR(3) BINARY NOT NULL,
  /* the currency code */
  `currency_code` VARCHAR(3) BINARY NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The country neighbor defines what countries
are neigbors with each other */
CREATE TABLE IF NOT EXISTS awa_country_neighbor (
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `neighbor_of_id` BIGINT NOT NULL,
  /*  */
  `neighbor_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* Region defines an area within a country. */
CREATE TABLE IF NOT EXISTS awa_region (
  /* the region identifier */
  `id` BIGINT NOT NULL,
  /* the region name */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the region geonameid */
  `geonameid` INTEGER NOT NULL,
  /* the country that this region belongs to */
  `country_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT IGNORE INTO entity_type (name) VALUES
("awa_city"), ("awa_country"), ("awa_country_neighbor"), ("awa_region");
