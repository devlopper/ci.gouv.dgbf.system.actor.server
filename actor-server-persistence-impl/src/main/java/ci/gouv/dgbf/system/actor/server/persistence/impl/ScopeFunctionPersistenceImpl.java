package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ScopeFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<ScopeFunction> implements ScopeFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}