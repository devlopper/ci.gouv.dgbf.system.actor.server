package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.IdentificationFormAttributBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.IdentificationFormAttributPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribut;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class IdentificationFormAttributBusinessImpl extends AbstractBusinessEntityImpl<IdentificationFormAttribut, IdentificationFormAttributPersistence> implements IdentificationFormAttributBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
