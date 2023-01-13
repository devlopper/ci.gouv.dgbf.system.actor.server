package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.CountryBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.CountryPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Country;

@ApplicationScoped
public class CountryBusinessImpl extends AbstractBusinessEntityImpl<Country, CountryPersistence> implements CountryBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
