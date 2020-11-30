package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.IdentificationFormAttributeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.IdentificationFormAttributePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribute;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class IdentificationFormAttributeBusinessImpl extends AbstractBusinessEntityImpl<IdentificationFormAttribute, IdentificationFormAttributePersistence> implements IdentificationFormAttributeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
