package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.IdentificationAttributPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribut;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class IdentificationAttributPersistenceImpl extends AbstractPersistenceEntityImpl<IdentificationAttribut> implements IdentificationAttributPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}