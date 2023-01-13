package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

import ci.gouv.dgbf.system.actor.server.persistence.api.CountryPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Country;

@ApplicationScoped
public class CountryPersistenceImpl extends AbstractPersistenceEntityImpl<Country> implements CountryPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}