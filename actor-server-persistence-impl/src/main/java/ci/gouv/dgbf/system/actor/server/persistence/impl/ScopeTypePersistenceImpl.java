package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ScopeTypePersistenceImpl extends AbstractPersistenceEntityImpl<ScopeType> implements ScopeTypePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}