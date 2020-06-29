package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.AdministrativeUnitBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AdministrativeUnitPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class AdministrativeUnitBusinessImpl extends AbstractBusinessEntityImpl<AdministrativeUnit, AdministrativeUnitPersistence> implements AdministrativeUnitBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
