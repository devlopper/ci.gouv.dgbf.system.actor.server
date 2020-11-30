package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.IdentificationFormAttributePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribute;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class IdentificationFormAttributePersistenceImpl extends AbstractPersistenceEntityImpl<IdentificationFormAttribute> implements IdentificationFormAttributePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}