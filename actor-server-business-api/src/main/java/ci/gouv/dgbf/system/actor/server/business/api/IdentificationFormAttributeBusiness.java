package ci.gouv.dgbf.system.actor.server.business.api;

import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribute;
import org.cyk.utility.server.business.BusinessEntity;

public interface IdentificationFormAttributeBusiness extends BusinessEntity<IdentificationFormAttribute> {

	String SAVE = "IdentificationFormAttribute.save";
	
}
