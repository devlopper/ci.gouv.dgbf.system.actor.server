package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.IdentificationAttributePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribute;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class IdentificationAttributePersistenceImpl extends AbstractPersistenceEntityImpl<IdentificationAttribute> implements IdentificationAttributePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}