package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.FinancialControllerServiceBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.FinancialControllerServicePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FinancialControllerService;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class FinancialControllerServiceBusinessImpl extends AbstractBusinessEntityImpl<FinancialControllerService, FinancialControllerServicePersistence> implements FinancialControllerServiceBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
