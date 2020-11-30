package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.IdentificationAttributeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.IdentificationAttributePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribute;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class IdentificationAttributeBusinessImpl extends AbstractBusinessEntityImpl<IdentificationAttribute, IdentificationAttributePersistence> implements IdentificationAttributeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
