package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.FinancialControllerServiceRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.FinancialControllerServiceDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class FinancialControllerServiceRepresentationImpl extends AbstractRepresentationEntityImpl<FinancialControllerServiceDto> implements FinancialControllerServiceRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
