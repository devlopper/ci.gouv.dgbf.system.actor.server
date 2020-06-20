package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.FunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class FunctionPersistenceImpl extends AbstractPersistenceEntityImpl<Function> implements FunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}