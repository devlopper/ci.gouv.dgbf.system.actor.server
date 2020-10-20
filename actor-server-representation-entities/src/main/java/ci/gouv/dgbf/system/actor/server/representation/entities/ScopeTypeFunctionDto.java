package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ScopeTypeFunctionDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ScopeTypeDto scopeType;
	private String scopeTypeAsString;
	private FunctionDto function;
	private String functionAsString;
	private Boolean scopeFunctionDerivable;	
	private String scopeFunctionDerivableAsString;
	
	@Override
	public ScopeTypeFunctionDto setIdentifier(String identifier) {
		return (ScopeTypeFunctionDto) super.setIdentifier(identifier);
	}
	
	@Override
	public ScopeTypeFunctionDto set__deletable__(Boolean __deletable__) {
		return (ScopeTypeFunctionDto) super.set__deletable__(__deletable__);
	}
}