package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.IdentificationAttributBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.IdentificationAttributPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribut;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class IdentificationAttributBusinessImpl extends AbstractBusinessEntityImpl<IdentificationAttribut, IdentificationAttributPersistence> implements IdentificationAttributBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
