package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.IdentificationFormAttributPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribut;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class IdentificationFormAttributPersistenceImpl extends AbstractPersistenceEntityImpl<IdentificationFormAttribut> implements IdentificationFormAttributPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}