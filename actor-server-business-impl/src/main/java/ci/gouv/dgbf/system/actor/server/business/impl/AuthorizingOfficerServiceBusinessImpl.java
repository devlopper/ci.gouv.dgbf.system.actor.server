package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.AuthorizingOfficerServiceBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.AuthorizingOfficerServicePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AuthorizingOfficerService;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class AuthorizingOfficerServiceBusinessImpl extends AbstractBusinessEntityImpl<AuthorizingOfficerService, AuthorizingOfficerServicePersistence> implements AuthorizingOfficerServiceBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
