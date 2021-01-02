package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.AuthorizingOfficerServicePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AuthorizingOfficerService;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class AuthorizingOfficerServicePersistenceImpl extends AbstractPersistenceEntityImpl<AuthorizingOfficerService> implements AuthorizingOfficerServicePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}