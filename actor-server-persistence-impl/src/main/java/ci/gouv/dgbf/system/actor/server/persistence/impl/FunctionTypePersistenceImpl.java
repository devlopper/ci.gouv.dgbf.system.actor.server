package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.FunctionTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class FunctionTypePersistenceImpl extends AbstractPersistenceEntityImpl<FunctionType> implements FunctionTypePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}