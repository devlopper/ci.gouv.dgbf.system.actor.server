package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ScopePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ScopePersistenceImpl extends AbstractPersistenceEntityImpl<Scope> implements ScopePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}