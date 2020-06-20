package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ProfileFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ProfileFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<ProfileFunction> implements ProfileFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}