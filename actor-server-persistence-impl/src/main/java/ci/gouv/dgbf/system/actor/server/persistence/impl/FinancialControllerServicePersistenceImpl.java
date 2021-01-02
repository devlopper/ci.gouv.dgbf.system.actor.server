package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.FinancialControllerServicePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.FinancialControllerService;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class FinancialControllerServicePersistenceImpl extends AbstractPersistenceEntityImpl<FinancialControllerService> implements FinancialControllerServicePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}